
# Sims City

Sims City en Haskell-SDL2.

## Compilation

Ce projet requiert la bibliothèque sdl2 (Simple Media Library, v2.0).

### Sous Linux ou MacOS

Il suffit d'installer la dépendance associée (par exemple `libsdl2-dev` sous Debian/Ubuntu).

**Remarque**: SDL2 ne semble pas encore compatible avec la puce M1 des nouveaux MAC.

### Sous Windows

C'est un peu plus complexe (comme d'habitude). Le plus simple est de passer par *msys2* dont une version est installée par *stack*. Normalement, la commande suivante devrait suffire :


### Construction du projet

Dans tous les cas, on utilisera :

```
stack build
```

Pour construire le projet.

### Lancer le jeu

Et :

```
stack run
```

