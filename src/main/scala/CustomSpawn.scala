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

  case object External extends Status // External to the bubble
  case object Bubble extends Status // Within the bubble
  case object Output extends Status // Within the bubble and bubble output producer

  type Proc[A, B, C] = A => B => (C, Status)

  case class ProcInstance[A, B, C](params: A)(val proc: Proc[A, B, C], val value: Option[(C, Status)] = None)
  {
    def run(args: B) =
      ProcInstance(params)(proc, { align(puid) { _ => Some(proc.apply(params)(args)) } })

    override def toString: String =
      s"{params:($params), val:($value)}"

    val puid: String = s"procInstance_${params.hashCode()}"
  }

  def spawn[A, B, C](process: Proc[A, B, C], params: Set[A], args: B): Map[A,C] = {
    rep(Map[A, ProcInstance[A, B, C]]()) { case currProcs => {
      // 1. Take active process instances from my neighbours
      val nbrProcs = excludingSelf.unionHoodSet(nbr(currProcs.keySet))
        .map(pi => pi -> ProcInstance(pi)(process)).toMap // TODO: should assume serialization of object

      // 2. New processes to be spawn, based on a generation condition
      val newProcs = params.map { case arg => arg -> ProcInstance(arg)(process) }.toMap

      // 3. Collect all process instances to be executed, execute them and update their state
      (currProcs ++ nbrProcs ++ newProcs)
        .mapValuesStrict(p => {
          vm.newExportStack
          val result = p.run(args)
          if(result.value.get._2 == External) vm.discardExport else vm.mergeExport
          result
        })
        .filterValues(_.value.get._2 != External)
    } }.collect { case (k, p) if p.value.get._2 == Output => k -> p.value.get._1 }
  }

  implicit class RichMap[K,V](val m: Map[K,V]){
    def filterValues(pred: V => Boolean): Map[K,V] =
      m.filter { case (k,v) => pred(v) }

    def mapValuesStrict[U](mapLogic: V => U): Map[K,U] =
      m.map { case (k,v) => k -> mapLogic(v) }
  }
}
