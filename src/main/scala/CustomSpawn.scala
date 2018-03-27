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

  sealed case class Scope[K](key: K) extends Slot

  case class ProcInstance[A, B, C](puid: PUID)
                                  (val params: A, val proc: Proc[A, B, C], val value: Option[(C, Status)] = None)
  //                                (implicit val aggregateProgram: ExecutionTemplate)
  {
    //import aggregateProgram._
    def run(args: B) = ProcInstance(puid)(params, proc, align(puid) { _ => {
      val v = Some(proc.apply(params)(args))
      //env.put(s"Proc$puid", s"Dev $mid running $puid (params: $params) with args $args => res: $v")
      v
    } })

    override def toString: String = {
      s"{$puid, params:($params), val:($value)}"
    }
  }

  def spawn[A, B, C](process: Proc[A, B, C], params: List[A], args: B): Iterable[C] = {
    val procs = rep((0, Map[PUID, ProcInstance[A, B, C]]())) { case (k, currProcs) => {
      // 1. Take previous processes (from me and from my neighbours)
      val nbrProcs = excludingSelf.mergeHoodFirst(nbr(currProcs))
        .mapValues(pi => ProcInstance(pi.puid)(pi.params, process))

      // 2. New processes to be spawn, based on a generation condition
      val newProcs = params.zipWithIndex.map { case (arg, i) => {
        val id = PUID(s"${mid}_${k + i}")
        val newProc = ProcInstance(id)(arg, process)
        id -> newProc
      }
      }.toMap

      val allprocs = (currProcs ++ nbrProcs ++ newProcs)
      env.put(s"Spawn result $k: ", allprocs)

      // 3. Collect all process instances to be executed, execute them and update their state
      (k + params.length, allprocs
        .mapValuesStrict(p => p.run(args))
        .filterValues(_.value.get._2 != External))
    } }
    procs._2.collect { case (_, p) if p.value.get._2 == Output => p.value.get._1 }
  }

  implicit class RichMap[K,V](val m: Map[K,V]){
    def filterValues(pred: V => Boolean): Map[K,V] =
      m.filter { case (k,v) => pred(v) }

    def mapValuesStrict[U](mapLogic: V => U): Map[K,U] =
      m.map { case (k,v) => k -> mapLogic(v) }
  }

  /**
    * Process (kind) identifier
    * @param pid
    */
  case class PID(pid: String){
    override def toString: String = s"pid$pid"
  }

  /**
    * Process instance identifier
    * @param puid
    */
  case class PUID(puid: String){
    override def toString: String = s"puid$puid"
  }
}
