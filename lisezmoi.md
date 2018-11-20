
*Eschecs* est un jeu d'échecs gratuit, qui utilise le [protocole UCI](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html) pour dialoguer avec des joueurs d'échecs artificiels.

*Eschecs* inclut un module original, appelé *Moustique*. *Eschecs* et *Moustique* sont des programmes à code source ouvert écrits en Pascal.

## I. Guide de l'utilisateur

### A. Déplacement des pièces

Pour déplacer une pièce, faites-la glisser avec la souris jusqu'à la case d'arrivée.

### B. Adversaire artificiel

Au lancement de l'application, le moteur par défaut est chargé (Fruit, ou le dernier moteur utlisé). Vous pouvez choisir un adversaire dans la liste des modules, dans le menu "Coups".

### C. Modes de jeu

Par défaut, l'utilisateur a les blancs et l'ordinateur les noirs. 

Si vous souhaitez que l'ordinateur prenne les blancs, cliquez sur "Coup de l'ordinateur", dans le menu "Coups".

Pour jouer humain contre humain, décochez l'option "Réponse automatique", dans le menu "Coups".

### D. Sauvegarde de la partie et des préférences

Si vous quittez *Eschecs* en passant par le menu "Fichier" ou en pressant la touche Échap, la partie et les options sont automatiquement enregistrées.

### E. Contrôles clavier

    ↑   Aller à la dernière position
    ↓   Aller à la première position
    ←   Aller à la position précédente
    →   Aller à la position suivante
    ESC Quitter le programme

## I. Guide de l'utilisateur avancé

### A. Installer un nouveau moteur UCI.

À faire.

### B. Protocole.

À faire.

## III. Guide du programmeur

### A. Compilation

*Eschecs* est un programme pour le compilateur Free Pascal. *Eschecs* utilise les bibliothèques [fpGUI][1] et [BGRABitmap][2].

## IV. Crédits

L'échiquier de 320 sur 320 (avec ses pièces) est une reproduction du jeu *[Fritz 1.0]*. *Fritz* est un programme d'échecs de Mathias Feist, Frans Morsch et Mathias Wüllenweber.

L'échiquier de 640 est [l'œuvre de Daniela Di Lena](https://dilena.de/chess-artwork-pieces-and-board-art-assets).

Les pièces de l'échiquier de 480 proviennent de la police Chess Mark d'Armando Marroquin.

L'icône d'*Eschecs* vient de la police [Chess Montreal](http://alcor.concordia.ca/~gpkatch/montreal_font.html) de Gary Katch.

*Moustique* est un programme d'échecs de Roland Chastain, basé sur *JSChess* de Jürgen Schlottke et sur *Mater* de Valentin Albillo.

## V. Auteur

*Eschecs* est un programme en Pascal écrit par Fred van Stappen et Roland Chastain.

Contributeurs : Johann Elsass.

## VI. Site internet

<https://sites.google.com/view/eschecs/accueil> 

[1]: https://github.com/graemeg/fpGUI 
[2]: https://github.com/bgrabitmap/bgrabitmap
[Fritz 1.0]: http://www.top-5000.nl/cp.htm
