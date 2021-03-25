#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Exemple utilisation de YAML

Created on Thu Mar 25 19:05:19 2021

@author: jeremylhour
"""
import yaml
import os


def fonction_exemple(methode):
    """
    fonction_exemple:
        fonction pour montrer l'utilisation de YAML
    
    @param method (str): nom de la methode
    """
    methodes_possibles = ['methode1', 'methode2']
    if not methode in methodes_possibles:
        print(f'Aucune methode {methode}')
        return None
    
    if methode == 'methode1':
        print('Vous avez choisi la méthode 1.')
        return None
    
    elif methode == 'methode2':
        print('vous avez choisi la méthode 2.')
        return None


if __name__ == '__main__':
    config_file = os.path.join(os.getcwd(),'exempleConfigYAML.yml') # chemin du fichier de configuration

    with open(config_file, 'r') as stream: # ouverture
        config = yaml.safe_load(stream)

    fonction_exemple(methode = config['methode']) # appel à la fonction avec la méthode demandée