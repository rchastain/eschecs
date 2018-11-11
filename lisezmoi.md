
*Eschecs* est un jeu d'échecs gratuit pour le système d'exploitation Windows.

*Eschecs* utilise le [protocole UCI](http://www.shredderchess.com/chess-info/features/uci-universal-chess-interface.html) pour dialoguer avec des modules externes qui sont autant d'adversaires pour l'utilisateur.

*Eschecs* inclut un module original, appelé *Moustique*.

*Eschecs* et *Moustique* sont des programmes à code source ouvert écrits en Pascal.

## I. Guide de l'utilisateur

### A. Déplacement des pièces

Pour déplacer une pièce, faites-la glisser avec la souris jusqu'à la case d'arrivée.

### B. Adversaire artificiel

Après avoir lancé l'application, vous devez choisir un adversaire dans la liste des modules, dans le menu "Coups". Tant que vous ne l'avez pas fait, l'ordinateur ne jouera pas !

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

Pour installer un nouveau moteur, vous devez éditer par vos propres moyens le fichier **engines.json**, qui contient les données dont *Eschecs* a besoin pour communiquer avec le moteur.

Si vous préférez ne pas modifier le fichier **engines.json**, vous pouvez créer un autre fichier qui s'appellera par exemple **autres_moteurs.json**, et dont le contenu se présentera ainsi :

    [
      {
        "command" : "bidule.exe",
        "name" : "Bidule",
        "protocol" : "uci",
        "workingDirectory" : "\\engines\\bidule\\"
      }
    ]

Vous devrez alors démarrer *Eschecs* en lui passant comme premier paramètre le nom du fichier.

J'ai supposé dans cet exemple que votre moteur serait placé comme les autres dans le répertoire **engines**, mais ce n'est pas une obligation. Le chemin du dossier peut être un chemin relatif (comme dans l'exemple) ou un chemin absolu.

### B. Protocole.

À faire.

## III. Guide du programmeur

### A. Compilation

*Eschecs* est un programme pour le compilateur Free Pascal.

L'interface graphique d'*Eschecs* est basée sur les bibliothèques [fpGUI][1] et [BGRABitmap][2].

Pour compiler *Eschecs*, ouvrez l'invite de commande en double-cliquant sur **comspec.cmd**. Dans l'invite de commande, tapez "build.lib" pour compiler fpGUI, LazUtils et BGRABitmap.

Pour compiler le programme lui-même, tapez "build". Si vous préférez compiler la version anglaise, tapez "build english".

Le chemin du compilateur doit être connu par le système. Autrement, pourvu que vous disposiez de l'EDI Lazarus, vous pouvez aussi ouvrir le projet **eschecs.lpi** que vous trouverez dans le dossier **sources**.

### B. Système d'exploitation.

Dans sa version actuelle, *Eschecs* n'est compatible qu'avec le système d'exploitation Windows. L'unité **ProcessUtils** s'appuie en effet sur l'unité **Windows**.

## IV. Crédits

Les pièces sont une reproduction de celles de *[Fritz 1.0]*.

*Fritz* est un programme d'échecs de Mathias Feist, Frans Morsch et Mathias Wüllenweber.

L'icône d'*Eschecs* est basée sur un caractère de la police Chess Montreal de Gary Katch.

<http://alcor.concordia.ca/~gpkatch/montreal_font.html>

*Booot* est un programme d'échecs d'Alex Morozov.
*Dumb* est un programme d'échecs de Richard Delorme.
*Fruit* est un programme d'échecs de Fabien Letouzey.
*Hermann* est un programme d'échecs de Volker Annuss.

*Moustique* est un programme d'échecs de Roland Chastain, basé sur *JSChess* de Jürgen Schlottke et sur *Mater* de Valentin Albillo. Le livre d'ouvertures de *ProDeo* est l'œuvre de Jeroen Noomen. Le programme permettant d'utiliser le livre est l'œuvre d'Ed Schröder, l'auteur de *ProDeo*.

## V. Auteur

*Eschecs* est un programme en Pascal écrit par Roland Chastain (eschecs2018@gmail.com).

## VI. Site internet

<https://sites.google.com/view/eschecs/accueil> 

[1]: https://github.com/graemeg/fpGUI 
[2]: https://github.com/bgrabitmap/bgrabitmap
[Fritz 1.0]: http://www.top-5000.nl/cp.htm
