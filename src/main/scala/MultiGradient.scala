import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class MultiGradient extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn with BlockT with BlockG with BlockC {
  override type MainResult = Any

  val (idSource1, idSource2, generatorId) = (0, 378, 189)
  val timeTurnOffSource2 = 100
  val startEvery = 50 // A generation impulse every 50 time units
  val numActiveProcs = 4
  val considerAfter = 10 // Consider this process output only after 10 time units

  def source = mid()==idSource1 | (mid()==idSource2 & timer(timeTurnOffSource2)>0)

  def isGenerator = mid==generatorId

  def replicatedGossip2(src: Boolean, numActiveProcs: Int, startEvery: Int, considerAfter: Int): Double = {
   val procs = spawn[Unit,Boolean,Double]( (_) => source => {
     val shutdownLimit = timer(startEvery * numActiveProcs + startEvery/2 + considerAfter)
     val outlimit = timer(considerAfter)
     //env.put("P>Shutdown limit", shutdownLimit)
     //env.put("P>Out limit", outlimit)
      val status = mux[Status](shutdownLimit!=0){
        mux[Status](outlimit!=0){ Bubble }{ Output }
      }{ External }
     //env.put("P>Status", status)
      (distanceTo(source), status)
    }, mux(isGenerator & impulsesEvery(startEvery)){ List(()) }{ List() },
      src)
    env.put("Procs", procs)
    procs.lastOption.getOrElse(Double.PositiveInfinity)
  }
  
  /*****************************
  ** MAIN PROGRAM ENTRY POINT **
  ******************************/

  override def main() = {
    val src = source
    env.put("source", src)
    env.put("generator", isGenerator)
    env.put("plain-gradient", distanceTo(src))
    replicatedGossip2(src, startEvery, numActiveProcs, considerAfter)
  }
}