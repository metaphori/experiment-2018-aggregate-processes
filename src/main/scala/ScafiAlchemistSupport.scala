import it.unibo.alchemist.implementation.nodes.NodeManager
import it.unibo.alchemist.model.interfaces.Time

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


trait ScafiAlchemistSupport { self: AggregateProgram with StandardSensors =>
  def env = sense[NodeManager]("manager")
  def currTime: Double = sense[Time]("time").toDouble()
  def dt(): Double = sense[Time]("dt").toDouble()
  def nextRandom: Double = sense[()=>java.lang.Double]("random")().toDouble
}
