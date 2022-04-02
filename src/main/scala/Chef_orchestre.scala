package upmc.akka.ppc

import akka.actor._

object ElectionChef {
    abstract class Musicien_Statut
    case class Passive() extends Musicien_Statut
    case class Candidate() extends Musicien_Statut
    case class Dummy() extends Musicien_Statut
    case class Waiting() extends Musicien_Statut
    case class Leader() extends Musicien_Statut

    abstract class LeaderAlgoMessage
    case class Initiate() extends LeaderAlgoMessage
    case class ALG(list : List[Int], init: Int) extends LeaderAlgoMessage
    case class AVS(list: List[Int], j: Int) extends LeaderAlgoMessage
    case class AVSRSP(list: List[Int], k: Int) extends LeaderAlgoMessage
    case class StartElection() extends LeaderAlgoMessage
    case class Changement_Chef(id: Int) extends LeaderAlgoMessage
    case class Start() extends LeaderAlgoMessage
    case class LeaderElectionChanged(id:Int) extends LeaderAlgoMessage

}



case class StartWithNodeList(list: List[Int])


class ElectionChef(val id: Int, val terminaux: List[Terminal]) extends Actor {
    import  ElectionChef._
    
    val father = context.parent
    var nodesAlive: List[Int] = List(id)

    var status: Musicien_Statut = new Passive
    var successeur = -1
    var predecesseur = -1
    var electionActorNeigh: ActorSelection = null


    def get_actor_selection(n:Int):ActorSelection = {
        electionActorNeigh = context.actorSelection("akka.tcp://MozartSystem" +  terminaux(n).id + "@" +  terminaux(n).ip + ":" +  terminaux(n).port + "/user/Musicien/electionChef")
        return electionActorNeigh            
    }



    
    def receive: PartialFunction[Any, Unit] = {

        case LeaderElectionChanged(nodeId) =>{
            if(nodeId!=id){
                status = new Passive
                predecesseur = -1
                successeur = -1

            }
        }

        case StartWithNodeList(list) => {
            if (list.isEmpty) {
                this.nodesAlive = this.nodesAlive ::: List(id)
            }
            else {
                this.nodesAlive = list
            }
            if(this.nodesAlive.size == 1){
                status = new Leader
                father ! Changement_Chef(id)
            }
            else{
                status = new Candidate
                self ! Initiate
            }
        }

        case Initiate => {
          
            
            println("Suffrage d'un nouveau BOSS")
            status = new Candidate
            predecesseur = -1
            successeur = -1
            

            var voisin = (nodesAlive.indexOf(id) + 1) % nodesAlive.size
            
            electionActorNeigh = get_actor_selection(nodesAlive(voisin))
            electionActorNeigh ! ALG(nodesAlive, id)

        }

        case ALG(list, init) => {
            nodesAlive = list
            if (status.isInstanceOf[Passive]) {
                status = new Dummy
                val neigh = (list.indexOf(id) + 1) % list.size
                electionActorNeigh = get_actor_selection(nodesAlive(neigh))
                electionActorNeigh ! ALG(list, init)
            }
            if (status.isInstanceOf[Candidate]) {
                predecesseur = init
                if (id > init) {
                    if (successeur == -1) {
                        status = new Waiting
                        electionActorNeigh = get_actor_selection(init)
                        electionActorNeigh ! AVS(list, id)
                    } 
                    else {
                        electionActorNeigh = get_actor_selection(successeur)
                        electionActorNeigh ! AVSRSP(list, predecesseur)
                        status = new Dummy
                    }
                }
                if (init == id) {
                    status = new Leader
                    father ! Changement_Chef(id)
                }
            }
        }

        case AVS(list, j) => {
            nodesAlive = list
            if (status.isInstanceOf[Candidate]) {
                if (predecesseur == -1) successeur = j
                else {
                    electionActorNeigh = get_actor_selection(j)
                    electionActorNeigh ! AVSRSP(list, predecesseur)
                    status = new Dummy
                }
            }
            if (status.isInstanceOf[Waiting]) successeur = j
        }

        case AVSRSP(list, k) => {
            //father ! Message("Dans AVSRSP "+id)
            nodesAlive = list
            if (status.isInstanceOf[Waiting]) {
                if (id == k) {
                    //father ! Message("elu dans AVSRSP")
                    status = new Leader
                    father ! Changement_Chef(id)
                }
                else {
                    predecesseur = k
                    if(successeur == -1){
                        if (k < id) {
                            electionActorNeigh = get_actor_selection(k)
                            status = new Waiting
                            electionActorNeigh ! AVS(list, k)
                        }
                    } else {
                        status = new Dummy
                        electionActorNeigh =get_actor_selection(successeur)
                        electionActorNeigh ! AVSRSP(list, k)
                    }
                }
            }
        }

    }

}