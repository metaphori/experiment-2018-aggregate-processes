package chat.exp

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

/*
 * Copyright (C) 2016-2017, Roberto Casadei, Mirko Viroli, and contributors.
 * See the LICENCE.txt file distributed with this work for additional
 * information regarding copyright ownership.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
*/

trait CustomSpawn {
  self: AggregateProgram with ScafiAlchemistSupport with FieldUtils =>

  trait Status

  case object External extends Status   // External to the bubble
  case object Bubble extends Status     // Within the bubble
  case object Output extends Status     // Within the bubble and bubble output producer
  case object Terminated extends Status // Notifies the willingness to terminate the bubble

  case class ProcInstance[A, B, C](params: A)(val proc: A => B => C, val value: Option[C] = None)
  {
    def run(args: B) =
      ProcInstance(params)(proc, { align(puid) { _ => Some(proc.apply(params)(args)) } })

    override def toString: String =
      s"{params:($params), val:($value)}"

    val puid: String = s"procInstance_${params.hashCode()}"
  }

  def nbrpath[A](path: Path)(expr: => A): A = {
    val tvm = vm.asInstanceOf[RoundVMImpl]
    vm.nest(Nbr[A](vm.index))(vm.neighbour.map(_ == vm.self).getOrElse(false)) {
      vm.neighbour match {
        case Some(nbr) if (nbr != vm.self) => tvm.context
          .readSlot[A](vm.neighbour.get, path)
          .getOrElse(throw new OutOfDomainException(tvm.context.selfId, vm.neighbour.get, path))
        case _ => expr
      }
    }
  }

  def share[A](init: => A)(f: (A, () => A) => A): A = {
    rep(init){ oldRep =>
      val repp = vm.asInstanceOf[RoundVMImpl].status.path
      f(oldRep, () => nbrpath(repp)(oldRep))
    }
  }

  def simpleSpawn[A, B, C](process: A => B => (C, Boolean), params: Set[A], args: B): Map[A,C] = {
    share(Map[A, C]()) { case (_, nbrProcesses) => {
      // 1. Take active process instances from my neighbours
      val nbrProcs = includingSelf.unionHoodSet(nbrProcesses().keySet)

      // 2. New processes to be spawn, based on a generation condition
      val newProcs = params

      // 3. Collect all process instances to be executed, execute them and update their state
      (nbrProcs ++ newProcs)
        .map { case arg =>
          val p = ProcInstance(arg)(process)
          vm.newExportStack
          val result = p.run(args)
          env.put(SIM_METRIC_N_PROCS_RUN, env.get[Double](SIM_METRIC_N_PROCS_RUN) + 1)
          if(result.value.get._2) vm.mergeExport else vm.discardExport
          arg -> result
        }.collect { case(p,pi) if pi.value.get._2 => p -> pi.value.get._1 }.toMap
    } }
  }

  def spawn[A, B, C](process: A => B => (C, Status), params: Set[A], args: B): Map[A,C] = {
    simpleSpawn[A,B,Option[C]]((p: A) => (a: B) => {
      val (finished, result, status) = rep((false, none[C], false)) { case (finished, _, _) => {
        val (result, status) = process(p)(a)
        val newFinished = status == Terminated | includingSelf.anyHood(nbr{finished})
        val terminated = includingSelf.everyHood(nbr{newFinished})
        val (newResult, newStatus) = (result, status) match {
          case _ if terminated     => (None, false)
          case (_,     External)   => (None, false)
          case (_,     Terminated) => (None, true)
          case (value, Output)     => (Some(value), true)
          case (_,     Bubble)     => (None, true)
        }
        (newFinished, newResult, newStatus)
      } }
      (result, status)
    }, params, args).collect { case (k, Some(p)) => k -> p }
  }

  /**********************************************
    *************** COOMPACT SPAWN **************
    *********************************************/

  trait MapFilter[V] {
    def value: V
    def filter: Boolean
  }
  case class SpawnReturn[C](value: C, status: Boolean) extends MapFilter[C] {
    override def filter = status
  }

  def compactSimpleSpawn[Key, Args, R](process: Key => Args => SpawnReturn[R], newProcesses: Set[Key], args: Args): Map[Key,R] =
    spreadKeys[Key,R](newProcesses){ key => process(key)(args) }

  def spreadKeys[K,R](newKeys: Set[K])(mapKeys: K => MapFilter[R]): Map[K,R] =
    share(Map[K,R]()) { case (_, nbrKeys) =>
      (includingSelf.unionHoodSet(nbrKeys().keySet) ++ newKeys)
        .foldLeft(Map.empty[K,R]) { (m,key) =>
          simplyReturn{
            align(s"${mapKeys.getClass.getName}_${key.hashCode}"){ _ => mapKeys(key) }
          }
            .filteringExport
            .iff(_.filter)
            .map(v => m + (key -> v.value)).getOrElse(m)
        }
    }

  def run[A,B,C](proc: A => B => SpawnReturn[C], params: A, args: B): SpawnReturn[C] =
    align(s"process_${params.hashCode}") { _ => proc(params)(args) }

  class IffContinuation[T](expr: => T){
    var filterExport: Boolean = false

    def filteringExport =
      new IffContinuation[T](expr){
        filterExport = true
      }

    def iff(pred: T => Boolean): Option[T] = {
      if(filterExport) vm.newExportStack
      val result = expr
      if(pred(result)){
        if(filterExport) vm.mergeExport
        Some(result)
      } else {
        if(filterExport) vm.discardExport
        None
      }
    }
  }

  def simplyReturn[T](expr: => T) = new IffContinuation[T](expr)

  def compactSpawn[A, B, C](process: A => B => (C, Status), params: Set[A], args: B): Map[A,C] = {
    compactSimpleSpawn[A,B,Option[C]]((p: A) => (a: B) => {
      val (finished, result, status) = rep((false, none[C], false)) { case (finished, _, _) => {
        val (result, status) = process(p)(a)
        val newFinished = status == Terminated | includingSelf.anyHood(nbr{finished})
        val terminated = includingSelf.everyHood(nbr{newFinished})
        val SpawnReturn(newResult, newStatus) = (result, status) match {
          case _ if terminated     => SpawnReturn(None, false)
          case (_,     External)   => SpawnReturn(None, false)
          case (_,     Terminated) => SpawnReturn(None, true)
          case (value, Output)     => SpawnReturn(Some(value), true)
          case (_,     Bubble)     => SpawnReturn(None, true)
        }
        (newFinished, newResult, newStatus)
      } }
      SpawnReturn(result, status)
    }, params, args).collect { case (k, Some(p)) => k -> p }
  }

  /**********************************************
    ******************* UTILS *******************
    *********************************************/

  implicit class RichFieldOps(fo: FieldOps) {
    def everyHood(p: => Boolean): Boolean = {
      fo.foldhoodTemplate(true)(_&&_)(nbr{p})
    }
  }

  implicit class RichMap[K,V](val m: Map[K,V]){
    def filterValues(pred: V => Boolean): Map[K,V] =
      m.filter { case (k,v) => pred(v) }

    def mapValuesStrict[U](mapLogic: V => U): Map[K,U] =
      m.map { case (k,v) => k -> mapLogic(v) }
  }

  private def none[T]: Option[T] = None
}
