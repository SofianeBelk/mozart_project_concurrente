<center>
<img src="https://www.musicologie.org/Biographies/m/mozart.jpg" width="100" height="100"> 
</center>

# Projet MozartGame 

#### Binome 
- BELKHIR Sofiane
- KHEDDAR Amine

## Question 1 :
    le schéma se trouve dans le ficher Schema.pdf.

## Question 2

Au début, on a choisi de mettre le musicien 0 comme chef d'orchestre.


Il attends une vingtaine de secondes si aucun autre musicien n'est en vue.


Si d'autres musiciens entrent en scène, alors le chef d'orchestre Commence à affecter des notes à ces musiciens.




Si le chef d'orchestre tombe en panne, alors il y aura une élection pour élire un nouveau partir de la liste des musiciens actifs.




L'algorithme d'élection est divisé en 3 principaux barrages qui s'appellent entre eux pour trouver le nouveau chef.


Sachant qu'on a utilisé la théorie du pair-to-pair pour élire ce dernier.




On a trouvé plusieurs algorithmes de type "Leader election algorithm" voici quelques exemples :


            -> Leader election in rings :
                -> Randomized (probabilistic) leader election
                -> Uniform algorithm
                -> Rings with unique IDs

            -> Leader election in a mesh :
                -> Torus
                -> Oriented mesh
                -> Unoriented mesh

            -> Election in hypercubes 
            -> Election in complete networks

            -> Universal leader election techniques :
                -> Shout
                -> Mega-Merge
                -> Yo-yo

Dans notre coté on a choisi l'algorithme qui nous a été présenter durant les précédentes années lors de l'etude des graphe décrit dans cette [aritcle](http://people.scs.carleton.ca/~santoro/LeaderCompleteLinear.pdf).

