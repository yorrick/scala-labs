package yorrick.dependencyinjection.macwire

import com.softwaremill.macwire._
import yorrick.dependencyinjection.macwire.shunting.{ShuntingModule, PointSwitcher, TrainShunter}
import yorrick.dependencyinjection.macwire.loading.{LoadingModule, TrainLoader}
import yorrick.dependencyinjection.macwire.station.StationModule


package shunting {
  class PointSwitcher()

  class TrainCarCoupler()

  class TrainShunter(pointSwitcher: PointSwitcher,
                     trainCarCoupler: TrainCarCoupler)

  trait ShuntingModule {
    lazy val pointSwitcher = wire[PointSwitcher]
    lazy val trainCarCoupler = wire[TrainCarCoupler]
    lazy val trainShunter = wire[TrainShunter]
}
}

package loading {
  class CraneController()

  class TrainLoader(craneController: CraneController,
                    pointSwitcher: PointSwitcher)

  trait LoadingModule {
    lazy val craneController = wire[CraneController]
    lazy val trainLoader = wire[TrainLoader]

    // dependency of the module
    def pointSwitcher: PointSwitcher
  }
}

package station {
  class TrainDispatch()

  class TrainStation(trainShunter: TrainShunter,
                     trainLoader: TrainLoader,
                     trainDispatch: TrainDispatch) {

    def prepareAndDispatchNextTrain(): Unit = {
      println(s"$trainShunter")
      println(s"$trainLoader")
      println(s"$trainDispatch")
    }
  }

  trait StationModule {
    lazy val trainDispatch = wire[TrainDispatch]
    lazy val trainStation = wire[TrainStation]

    // dependencies of the module
    def trainShunter: TrainShunter
    def trainLoader: TrainLoader
  }
}

object TrainStation extends App {
  val modules = new ShuntingModule
    with LoadingModule
    with StationModule

  modules.trainStation.prepareAndDispatchNextTrain()
}
