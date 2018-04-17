package it.unibo.alchemist.implementations.actions

import it.unibo.alchemist.model.implementations.actions.AbstractConfigurableMoveNode
import it.unibo.alchemist.model.implementations.movestrategies.routing.IgnoreStreets
import it.unibo.alchemist.model.implementations.movestrategies.speed.ConstantSpeed
import it.unibo.alchemist.model.interfaces._
import it.unibo.alchemist.model.interfaces.movestrategies.TargetSelectionStrategy
import it.unibo.alchemist.scala.PimpMyAlchemist._
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath.{atan2, cos, sin}

class RandomRoundTrip[T](environment: Environment[T], node: Node[T],
                         protected val reaction: Reaction[T],
                         rng: RandomGenerator,
                         baseX: Double, baseY: Double,
                         minX: Double, minY: Double,
                         maxX: Double, maxY: Double,
                         waypointCount: Int,
                         speed: Double) extends AbstractConfigurableMoveNode(
  environment, node,
  new IgnoreStreets,
  new TargetSelectionStrategy {
    private var stack = List[Position]()
    private val base = environment.makePosition(baseX, baseY)
    private def randomInRange(min: Double, max: Double) = rng.nextDouble() * (maxX - minX) - minX
    private def randomPos() = environment.makePosition(randomInRange(minX, maxX), randomInRange(minY, maxY))
    override def getTarget: Position = {
      if (stack.isEmpty) {
        stack = base :: (1 to waypointCount).map(_ => randomPos()).toList ::: base :: Nil
      }
      val result = stack.head
      stack = stack.tail
      result
    }
  },
  new ConstantSpeed(reaction, speed)
) {
  override def getDestination(source: Position, target: Position, maxWalk: Double): Position =
    if (source.getDistanceTo(target) <= maxWalk) target
    else {
      val vector = target - source
      val angle = atan2(vector.getCoordinate(1), vector.getCoordinate(0))
      environment.makePosition(maxWalk * cos(angle), maxWalk * sin(angle))
    }

  override def cloneAction(node: Node[T], reaction: Reaction[T]): Action[T] = ???
}
