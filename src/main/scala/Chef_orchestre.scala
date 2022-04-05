package upmc.akka.ppc

import akka.actor._

object ElectionChef {
    abstract class Musicien_Statut
    case class Not_Chef() extends Musicien_Statut
    case class CandidatChef() extends Musicien_Statut
    case class Dummy() extends Musicien_Statut
    case class FileAttente() extends Musicien_Statut
    case class Chef() extends Musicien_Statut

    abstract class ChefAlgoMessage
    case class Initiate() extends ChefAlgoMessage
    case class Premier_Barrage(list : List[Int], init: Int) extends ChefAlgoMessage
    case class Deuxieme_Barrage(list: List[Int], j: Int) extends ChefAlgoMessage
    case class Troisieme_Barrage(list: List[Int], k: Int) extends ChefAlgoMessage
    case class StartElection() extends ChefAlgoMessage
    case class Changement_Chef(id: Int) extends ChefAlgoMessage
    case class Start() extends ChefAlgoMessage
    case class ChefElectionChanged(id:Int) extends ChefAlgoMessage

}



case class StartWithNodeList(list: List[Int])


class ElectionChef(val id: Int, val terminaux: List[Terminal]) extends Actor {
    import  ElectionChef._
    
    val pere = context.parent
    var CurrentMusiciens: List[Int] = List(id)

    var status: Musicien_Statut = new Not_Chef
    var successeur = -1
    var predecesseur = -1
    var NeighborSearch: ActorSelection = null


    def get_actor_selection(n:Int):ActorSelection = {
        NeighborSearch = context.actorSelection("akka.tcp://MozartSystem" +  terminaux(n).id + "@" +  terminaux(n).ip + ":" +  terminaux(n).port + "/user/Musicien/electionChef")
        return NeighborSearch            
    }

    def receive: PartialFunction[Any, Unit] = {

        case ChefElectionChanged(idMusicien) =>{
            // je ne suis pas le prochain chef
            if(idMusicien!=id){
                status = new Not_Chef
                predecesseur = -1
                successeur = -1
            }
        }

        case StartWithNodeList(list) => {
            if (!list.isEmpty) {
                this.CurrentMusiciens = list
            }
            else {
                this.CurrentMusiciens = this.CurrentMusiciens ::: List(id)
            }
            if(this.CurrentMusiciens.size == 1){
                status = new Chef
                pere ! Changement_Chef(id)
            }
            else{
                status = new CandidatChef
                self ! Initiate
            }
        }

        case Initiate => {
            println("Suffrage d'un nouveau BOSS")

            //changement de mon statut
            status = new CandidatChef
            predecesseur = -1
            successeur = -1
        
            var voisin = (CurrentMusiciens.indexOf(id) + 1) % CurrentMusiciens.size
        
            NeighborSearch = get_actor_selection(CurrentMusiciens(voisin))
            NeighborSearch ! Premier_Barrage(CurrentMusiciens, id)

        }

        case Premier_Barrage(list, init) => {
            CurrentMusiciens = list
            if (status.isInstanceOf[CandidatChef]) {
                // init ==> l'id du dernier chef d'orchestre
                predecesseur = init
                if (id > init) {
                    if (successeur == -1) {
                        status = new FileAttente
                        NeighborSearch = get_actor_selection(init)
                        NeighborSearch ! Deuxieme_Barrage(list, id)
                    } 
                    else {
                        NeighborSearch = get_actor_selection(successeur)
                        NeighborSearch ! Troisieme_Barrage(list, predecesseur)
                        status = new Dummy
                    }
                }
                if (init == id) {
                    status = new Chef
                    pere ! Changement_Chef(id)
                }
            }

            if (status.isInstanceOf[Not_Chef]) {
                status = new Dummy
                val neigh = (list.indexOf(id) + 1) % list.size
                NeighborSearch = get_actor_selection(CurrentMusiciens(neigh))
                NeighborSearch ! Premier_Barrage(list, init)
            }
           
        }

        case Deuxieme_Barrage(list, j) => {
            CurrentMusiciens = list

            if (status.isInstanceOf[FileAttente]) successeur = j


            if (status.isInstanceOf[CandidatChef]) {
                if (predecesseur == -1) successeur = j
                else {
                    NeighborSearch = get_actor_selection(j)
                    NeighborSearch ! Troisieme_Barrage(list, predecesseur)
                    status = new Dummy
                }
            }
           
        }

        case Troisieme_Barrage(list, k) => {
            CurrentMusiciens = list
            if (status.isInstanceOf[FileAttente]) {
                if (id == k) {
                    status = new Chef
                    pere ! Changement_Chef(id)
                }
                else {
                    predecesseur = k
                    if(successeur == -1){
                        if (k < id) {
                            NeighborSearch = get_actor_selection(k)
                            status = new FileAttente
                            NeighborSearch ! Deuxieme_Barrage(list, k)
                        }
                    } else {
                        status = new Dummy
                        NeighborSearch =get_actor_selection(successeur)
                        NeighborSearch ! Troisieme_Barrage(list, k)
                    }
                }
            }
        }

    }

}