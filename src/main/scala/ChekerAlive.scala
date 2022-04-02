package upmc.akka.ppc

import java.util
import java.util.Date
import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


object CheckerAlive {
    abstract class Tick
    case class Ruling_Action() extends Tick
    case class StartAlive() extends Tick
    case class Active_Musician(id: Int) extends Tick
    case class IsAliveCheckerLeader(id: Int) extends Tick
    case class Changement_Chef2(id: Int) extends Tick
    case class Maj_Musicien() extends Tick
    case class Manage(num: Int) extends Tick
    case class Finish() extends Tick
}

class CheckerAlive(val id: Int, val terminaux: List[Terminal], electionActor: ActorRef) extends Actor {

    import CheckerAlive._

    var time: Int = 2000
    val father = context.parent
    val rand = scala.util.Random
    var activeM: List[Int] = List() //la lsite des musiciens qui sont vivants
    var listDates = scala.collection.mutable.Map[Int, Date]()
   
    var chef: Int = -1
    var timer: Int = 10
    var Actif: Boolean = false
    var Inactif: Boolean = false

    def receive = {

        case StartAlive => {
            self ! Ruling_Action
            self ! Maj_Musicien
        }        

    /////////////////////////////////////////////////////////////////////////////////////////////


        case Active_Musician(idM) => {
            if (activeM.contains(idM)) {
                val actualDate = new Date().getTime
                val newDate = new Date(actualDate + time)
                listDates(idM) = newDate
            }
            else {
                val actualDate = new Date().getTime
                activeM = idM :: activeM 
                println("Liste des musiciens actifs " + activeM + " }")
                val tmpdate = new Date(actualDate + time)
                listDates = listDates + (idM -> tmpdate)
            }
            activeM = activeM.sorted
        }

        case Maj_Musicien => {////////////////////////////////////////////////////////////////////////OK
            val scheduler = context.system.scheduler
            scheduler.schedule(3000 milliseconds, 1000 milliseconds) {
                if (id == chef && !Inactif) {
                    if(activeM.length == 1) {
                        Actif = false
                        println("Il reste " + timer + " seconde avant la fin")
                        timer -= 1
                        if(timer == 0){
                            Inactif = true
                            father ! Finish()
                        }
                    }
                    else {
                        timer = 10
                        Actif = true
                    }
                }
            }
        }
        /////////////////////////////////////////////////////////////////////////////////////////////

        case Ruling_Action => {
            var tmp = chef
            val scheduler = context.system.scheduler

            scheduler.schedule(time milliseconds, time milliseconds) {
                val actualDate = new Date()
                if(Inactif){self ! PoisonPill}
                if (chef != tmp) {tmp = chef}

                if(Actif && !Inactif){
                    var affectation: Int = activeM(rand.nextInt(activeM.length))
                    father ! Manage(affectation)
                }
                
                for((idM, date) <- listDates) {
                    if (date.before(actualDate)) {
                        if(idM != chef) println("Le musicien : "+idM+" est partit")
                        else            println("Le chef "+idM+" est partit")

                        activeM = activeM diff List(idM)
                        listDates = listDates.-(idM)
                        if(idM==chef) {electionActor ! StartWithNodeList(activeM)}
                    }
                }
            }
        }

        case Changement_Chef2(idM) => {chef = idM}
        case IsAliveCheckerLeader(idM) => {self ! Active_Musician(idM)}
    }
}