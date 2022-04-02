package upmc.akka.ppc



import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor._
import akka.io._
import akka.util.Timeout

import java.net._

object BroadcastSignal {
    import CheckerAlive._
    sealed trait BeatMessage
    case class Assert_Musicien(id: Int) extends BeatMessage
    case class Assert_Chef(id: Int) extends BeatMessage
    case class Check() extends Tick
    case class Start_Verification() extends BeatMessage
    case class Changement_Chef1(idM: Int)
}

class BroadcastSignal(val id: Int) extends Actor {
    import BroadcastSignal._
    import CheckerAlive._

    val time: Int = 1000
    val pere = context.parent
    var chef: Int = 0 

    def receive = {

        case Start_Verification => {
            self ! Check
            if (this.id == this.chef) {println("I am the boss")}
        }
        case Check => {
            val scheduler = context.system.scheduler
            scheduler.schedule(0 milliseconds, time milliseconds) {
                
                pere ! Assert_Musicien(id)

                if (chef == id) pere ! Assert_Chef(chef)
            }
        }

        case Changement_Chef1(idM) => chef = idM
    }

}