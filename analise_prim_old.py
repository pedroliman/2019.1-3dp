#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec 29 09:47:02 2017

@author: pedro
"""

# Exemplo da Análise PRIM Obtido em:
# https://github.com/Project-Platypus/PRIM

## Para instalar os pacotes com a versão do python que o R usou, tive que fazer isso no terminal:
# python3.6 -m pip install prim
# Depois disso ele resolveu permitir usar o anaconda!! Vai enteder...

import prim
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Gerando a Resposta:
resposta = pd.read_csv("profit_resposta.csv", index_col=0, parse_dates=True)
# Convertendo resposta em um array unidimensional
resposta = resposta["x"]
# Variaveis de Entrada:
incertezas = pd.read_csv("profit_incertezas.csv", index_col=0, parse_dates=True)

# Rodando a Análise com Incertezas no Shortlist:
p = prim.Prim(incertezas, resposta, threshold=0.9, threshold_type=">")

box = p.find_box()

box.show_tradeoff()

plt.show()

# Rodando a Análise com Todas as Incertezas no Shortlist:
p = prim.Prim(incertezas_completas, resposta, threshold=211920013, threshold_type=">")

box = p.find_box()

box.show_tradeoff()

plt.show()

# box.show_details()

aFractionalDiscardRate
aInitialReoderShare
aReferencePopulation


