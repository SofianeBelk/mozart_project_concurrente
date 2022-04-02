package upmc.akka.ppc

import akka.actor._
import akka.actor.{ ActorInitializationException, ActorKilledException, OneForOneStrategy }
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.util.Timeout

sealed trait SyncMessage
case class Sync(musiciens: List[Int]) extends SyncMessage
case class SyncForOneMusicien(musicienId: Int, musiciens: List[Int]) extends SyncMessage


object Musicien{
  case class StartMozart()
  case class IsAlive(id: Int)
  case class IsAlive_Chef(id: Int)
  case class Play(num: Int)
} 

class Musicien(val id: Int, val terminaux: List[Terminal], val systemActor: ActorSystem) extends Actor {
    import Musicien._
    import ProviderActor._
    import PlayerActor._
    import DataBaseActor._
    import CheckerAlive._
    import ElectionChef._
    import BroadcastSignal._

    // Les differents acteurs du systeme
    val electionChef = context.actorOf(Props(new ElectionChef(this.id, terminaux)), name = "electionChef")
    val beatActor = context.actorOf(Props(new BroadcastSignal(this.id)), name = "BroadcastSignal")
    val checkerAlive = context.actorOf(Props(new CheckerAlive(this.id, terminaux, electionChef)), name = "checkerAlive")
    val player = context.actorOf(Props[PlayerActor], "playerActor")
    val provider = context.actorOf(Props(new ProviderActor (self)), "provider")
    
    var ToutLesMusiciens: List[ActorSelection] = List()
    val TIME_BASE = 1800 milliseconds
    var des = 0

    override val supervisorStrategy = OneForOneStrategy() {case _ => Stop }

    def lancement : Int = {
        val r = new scala.util.Random
        val d1 = r.nextInt(5) + 1
        val d2 = r.nextInt(5) + 1
        d1 + d2
    }

    
    def receive = {

        case StartMozart   => {
            println("Entrée du musicien : "+ this.id)
            checkerAlive ! StartAlive
            beatActor ! Start_Verification
            
            terminaux.foreach(n => {
                if (n.id != id) {
                    val remote = context.actorSelection("akka.tcp://MozartSystem" + n.id + "@" + n.ip + ":" + n.port + "/user/Musicien")
                    this.ToutLesMusiciens = this.ToutLesMusiciens ::: List(remote)
                }
            })
            
        }

        case Manage(affectation) => {
            des = lancement
            println("Le musicien " + affectation+" a changé de morceaux")

            if(id == affectation){self ! Play(des)}

            else { 
                if(affectation > id){ToutLesMusiciens(affectation -1 )! Play(des)}
                else {ToutLesMusiciens(affectation) ! Play(des)}
            } 
        }

        case Changement_Chef(nodeId) => {
            beatActor ! Changement_Chef1(nodeId)
            checkerAlive ! Changement_Chef2(nodeId)
            electionChef ! LeaderElectionChanged(nodeId)
        }

        case Assert_Chef(nodeId) => {
            self ! IsAlive_Chef(nodeId)
            self ! Changement_Chef(nodeId)
            if(id == nodeId) { ToutLesMusiciens.foreach(node => {node ! Assert_Chef(nodeId)})}
        }

        case Assert_Musicien(nodeId) => {
            self ! IsAlive(nodeId)
            ToutLesMusiciens.foreach(node => {node ! IsAlive(nodeId)})
        }

        case Measure (chordlist)   => {player ! Measure (chordlist)}
        case IsAlive(id) => {checkerAlive ! Active_Musician(id)}
        case IsAlive_Chef(id) => { checkerAlive !IsAliveCheckerLeader(id)}
        case Play(num) => {provider ! GetMeasure (num)}
        case Finish() => {
            print("_________ Fin _______  ")
            self ! PoisonPill
        }
        
    }
}