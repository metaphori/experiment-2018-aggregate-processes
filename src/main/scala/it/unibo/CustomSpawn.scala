package it.unibo

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.chat.exp.Metrics

trait Spawn{
  def spawn[A, B, C](process: A => B => (C, Boolean), params: Set[A], args: B): Map[A,C]
}

object Spawn {
  trait Status

  case object ExternalStatus extends Status   // External to the bubble
  case object BubbleStatus extends Status     // Within the bubble
  case object OutputStatus extends Status     // Within the bubble and bubble output producer
  case object TerminatedStatus extends Status // Notifies the willingness to terminate the bubble

  val External: Status = ExternalStatus
  val Bubble: Status = BubbleStatus
  val Output: Status = OutputStatus
  val Terminated: Status = TerminatedStatus
}

trait CustomSpawn extends Spawn {
  self: AggregateProgram with ScafiAlchemistSupport with FieldUtils =>

  import Spawn._

  val SIM_METRIC_N_PROCS_RUN = "n_procs_run"

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

  def spawn[A, B, C](process: A => B => (C, Boolean), params: Set[A], args: B): Map[A,C] = {
    share(Map[A, C]()) { case (_, nbrProcesses) => {
      // 1. Take active process instances from my neighbours
      val nbrProcs = includingSelf.unionHoodSet(nbrProcesses().keySet)

      // 2. New processes to be spawn, based on a generation condition
      val newProcs = params

      // 3. Collect all process instances to be executed, execute them and update their state
      (nbrProcs ++ newProcs)
        .map { case arg =>
          val p = ProcInstance(arg)(a => {
            env.put(Metrics.BANDWIDTH_SPAWN, env.get[Double](Metrics.BANDWIDTH_SPAWN) + excludingSelf.sumHood(nbr(1)))
            process(a)
          })
          vm.newExportStack
          val result = p.run(args)
          env.put(Metrics.ACTIVE_PROCESSES, env.get[Double](Metrics.ACTIVE_PROCESSES) + 1)
          if(result.value.get._2) vm.mergeExport else vm.discardExport
          arg -> result
        }.collect { case(p,pi) if pi.value.get._2 => p -> pi.value.get._1 }.toMap
    } }
  }

  def sspawn[A, B, C](process: A => B => (C, Status), params: Set[A], args: B): Map[A,C] = {
    spawn[A,B,Option[C]]((p: A) => (a: B) => {
      val (finished, result, status) = rep((false, none[C], false)) { case (finished, _, _) => {
        val (result, status) = process(p)(a)
        val newFinished = status == Terminated | includingSelf.anyHood(nbr{finished})
        val terminated = includingSelf.everyHood(nbr{newFinished})
        val (newResult, newStatus) = (result, status) match {
          case _ if terminated     => env.put("bubble",0); (None, false)
          case (_,     External)   => env.put("bubble",0); (None, false)
          case (_,     Terminated) => env.put("bubble",3); (None, true)
          case (value, Output)     => env.put("bubble",2); (Some(value), true)
          case (_,     Bubble)     => env.put("bubble",1); (None, true)
        }
        (newFinished, newResult, newStatus)
      } }
      (result, status)
    }, params, args).collect { case (k, Some(p)) => k -> p }
  }

  object on {
    def apply[K](set: Set[K]) = new SpawnKeys(set)
  }
  class SpawnKeys[K](val keys: Set[K]) {
    def withArgs[Args](args: Args) = new SpawnContinuation(keys, args)
  }
  class SpawnContinuation[K,Args](val keys: Set[K], val args: Args){
    def spawn[R](proc: K => Args => (R,Status)): Map[K,R] =
      csspawn(proc, keys, args)
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

  // "Compact" spawn
  def cspawn[Key, Args, R](process: Key => Args => SpawnReturn[R], newProcesses: Set[Key], args: Args): Map[Key,R] =
    spreadKeys[Key,R](newProcesses){ key => env.put(SIM_METRIC_N_PROCS_RUN, env.get[Double](SIM_METRIC_N_PROCS_RUN) + 1); process(key)(args) }

  def spreadKeys[K,R](newKeys: Set[K])(mapKey: K => MapFilter[R]): Map[K,R] =
    share(Map[K,R]()) { case (_, nbrMaps) =>
      (includingSelf.unionHoodSet(nbrMaps().keySet) ++ newKeys).mapAndFilter[R]{ (key: K) =>
          simplyReturn(alignedExecution(mapKey)(key)).filteringExport.iff(_.filter).map(_.value)
      }
    }

  def cuspawn[K, A, R](process: K => A => SpawnReturn[R], newKeys: Set[K], args: A): Map[K,R] =
    share(Map[K,R]()) { case (_, nbrMaps) =>
      (includingSelf.unionHoodSet(nbrMaps().keySet) ++ newKeys).mapAndFilter[R]{ (key: K) =>
        simplyReturn(alignedExecution(process(_:K)(args))(key)).filteringExport.iff(_.filter).map(_.value)
      }
    }

  implicit class MyRichSet[K](val set: Set[K]) {
    def mapAndFilter[V](f: K => Option[V]): Map[K,V] =
      set.foldLeft(Map.empty[K,V]) { (m,key) =>
        f(key).map(v => m + (key -> v)).getOrElse(m)
      }
  }

  def alignedExecution[K,V](p: K => V)(key: K): V =
    align(s"${p.getClass.getName}_${key.hashCode}"){ _ => p(key) }

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

  // "Compact" "status" spawn
  def csspawn[A, B, C](process: A => B => (C, Status), params: Set[A], args: B): Map[A,C] = {
    cspawn[A,B,Option[C]]((p: A) => (a: B) => {
      val (finished, result, status) = rep((false, none[C], false)) { case (finished, _, _) => {
        val (result, status) = process(p)(a)
        val newFinished = status == Terminated | includingSelf.anyHood(nbr{finished})
        val terminated = includingSelf.everyHood(nbr{newFinished})
        val nnbrs = excludingSelf.sumHood(nbr(1))
        val SpawnReturn(newResult, newStatus) = (result, status) match {
          case _ if terminated     => SpawnReturn(None, false)
          case (_,     External)   => SpawnReturn(None, false)
          case (_,     Terminated) => env.put(Metrics.BANDWIDTH_SPAWN, env.get[Double](Metrics.BANDWIDTH_SPAWN) + nnbrs); SpawnReturn(None, true)
          case (value, Output)     => env.put(Metrics.BANDWIDTH_SPAWN, env.get[Double](Metrics.BANDWIDTH_SPAWN) + nnbrs); SpawnReturn(Some(value), true)
          case (_,     Bubble)     => env.put(Metrics.BANDWIDTH_SPAWN, env.get[Double](Metrics.BANDWIDTH_SPAWN) + nnbrs); SpawnReturn(None, true)
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
