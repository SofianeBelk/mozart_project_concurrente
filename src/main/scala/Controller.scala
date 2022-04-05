package upmc.akka.ppc

import java.util
import java.util.Date
import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


object Controller {
    abstract class Control
    case class Ruling_Action() extends Control
    case class StartAlive() extends Control
    case class Active_Musician(id: Int) extends Control
    case class IsAliveCheckerLeader(id: Int) extends Control
    case class Changement_Chef2(id: Int) extends Control
    case class Maj_Musicien() extends Control
    case class Manage(num: Int) extends Control
    case class Finish() extends Control
}

class Controller(val id: Int, val terminaux: List[Terminal], electionActor: ActorRef) extends Actor {

    import Controller._

    var time: Int = 2000
    val pere = context.parent
    val rand = scala.util.Random
    var activeM: List[Int] = List() //la lsite des musiciens qui sont vivants
    var listDates = scala.collection.mutable.Map[Int, Date]()
   
    var chef: Int = -1
    var timer: Int = 20
    var Actif: Boolean = false
    var Inactif: Boolean = false

    def receive = {

        case StartAlive => {
            self ! Ruling_Action
            self ! Maj_Musicien
        }        

    /////////////////////////////////////////////////////////////////////////////////////////////OK

        case Active_Musician(idM) => {

            if (activeM.contains(idM)) listDates(idM) = new Date(new Date().getTime + time)
            else {
                if(chef != idM){
                    activeM = idM :: activeM 
                }
                println("Liste des musiciens actifs " + activeM + " }")
                listDates = listDates + (idM -> new Date(new Date().getTime + time))
            }
            activeM = activeM.sorted
        }

        /////////////////////////////////////////////////////////////////////////////////////////////

        case Maj_Musicien => {
            val scheduler = context.system.scheduler
            scheduler.schedule(3000 milliseconds, 1000 milliseconds) {
                if (id == chef && !Inactif ) {
                    if(activeM.length == 1) {
                        Actif = false
                        println("Il reste " + timer + " seconde avant la fin")
                        timer =timer - 1
                        if(timer == 0){
                            Inactif = true
                            pere ! Finish()
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
                if(Inactif){
                    self ! PoisonPill
                } 
                if (chef != tmp) tmp = chef

                if(Actif && !Inactif){
                    var affectation: Int = activeM(rand.nextInt(activeM.length))
                    // println("le chef est "+chef)
                    while(affectation == chef)
                        affectation = activeM(rand.nextInt(activeM.length))
                    // println("affectation "+affectation)
                    pere ! Manage(affectation)
                }
               
                
                for((idM, date) <- listDates) {
                    if (date.before(actualDate)) {
                        if(idM != chef) println("Le musicien : "+idM+" est partit")
                        else            println("Le chef "+idM+" est partit")

                        listDates = listDates.-(idM)
                        activeM = activeM diff List(idM)
                        if(idM==chef) {electionActor ! StartWithNodeList(activeM)}
                    }
                }
            }
            activeM = activeM.filter(_!=chef)
        }


        case Changement_Chef2(idM) => {chef = idM}
        case IsAliveCheckerLeader(idM) => {self ! Active_Musician(idM)}
    }
}