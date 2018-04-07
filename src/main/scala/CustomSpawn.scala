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

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

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
    share(Map[A, ProcInstance[A, B, (C,Boolean)]]()) { case (currProcs, nbrProcesses) => {
      // 1. Take active process instances from my neighbours
      val repp = vm.asInstanceOf[RoundVMImpl].status.path
      val nbrProcs = excludingSelf.unionHoodSet(nbrProcesses().keySet)
        .map(pi => pi -> ProcInstance(pi)(process)).toMap // TODO: should assume serialization of object

      // 2. New processes to be spawn, based on a generation condition
      val newProcs = params.map { case arg => arg -> ProcInstance(arg)(process) }.toMap

      // 3. Collect all process instances to be executed, execute them and update their state
      (currProcs ++ nbrProcs ++ newProcs)
        .mapValuesStrict(p => {
          vm.newExportStack
          val result = p.run(args)
          if(result.value.get._2) vm.mergeExport else vm.discardExport
          result
        })
        .filterValues(_.value.get._2)
    } }.mapValuesStrict(_.value.get._1)
  }

  def spawn[A, B, C](process: A => B => (C, Status), params: Set[A], args: B): Map[A,C] = {
    simpleSpawn[A,B,Option[C]]((p: A) => (a: B) => {
      val (_, _, result, status) = rep((false, false, none[C], false)) { case (f, _, _, _) => {
        val (result, status) = process(p)(a)
        val terminated = includingSelf.everyHood(nbr{f})
        val (newResult, newStatus) = (result, status) match {
          case _ if terminated   => (None, false)
          case (value, Output)   => (Some(value), true)
          case (_,     Bubble)   => (None, true)
          case (_,     External) => (None, false)
        }
        (status == Terminated | includingSelf.anyHood(nbr{f}), terminated, newResult, newStatus)
      } }
      (result, status)
    }, params, args).collect { case (k, Some(p)) => k -> p }
  }

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
