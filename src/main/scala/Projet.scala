package upmc.akka.ppc

import akka.actor._
import com.typesafe.config.ConfigFactory

case class Terminal (id:Int, ip:String, port:Int)

object mozartGame {
    import Musicien._

    def main (args : Array[String]) {
        // Gestion des erreurs
          if (args.size != 1) {
               println ("Erreur de syntaxe : run <num>")
               sys.exit(1)
          }

          val id : Int = args(0).toInt

          if (id < 0 || id > 3) {
               println ("Erreur : <num> doit etre compris entre 0 et 3")
               sys.exit(1)
          }

          var musicienlist = List[Terminal]()
          
          // recuperation des adresses de tous les musiciens
          // hardcoded path name
          for(i <- 3 to 0 by -1){
               val address = "127.0.0.1"
               val port = ConfigFactory.load().getConfig("system"+ i).getValue("akka.remote.netty.tcp.port").render()
               musicienlist = Terminal(i, address, port.toInt)::musicienlist
          }

          println(musicienlist)


          // Initialisation du node <id>
          val system = ActorSystem("MozartSystem" + id, ConfigFactory.load().getConfig("system" + id))
          val musicien = system.actorOf(Props(new Musicien(id, musicienlist, system)), "Musicien")

          musicien ! StartMozart
        
    }



}