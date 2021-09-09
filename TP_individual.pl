%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - 2021

% TP INDIVIDUAL
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Maria Ramos, a89541
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

:- consult('grafo.pl').

% -------------------------------------------------------------------------------------------------------------------
% Predicados auxiliares


adjacente(X,Y,Custo) :- aresta(X,Y,Custo).

estimativa(X,Y,Distancia) :-
  ponto_recolha(X,(Lat1,Lng1),_,_,_),
  ponto_recolha(Y,(Lat2,Lng2),_,_,_),
  Distancia is sqrt(((Lat1-Lat2)^2) + ((Lng1-Lng2)^2)).

%-----------------------------------------------------------------

temLixo(Lixo,[(Lixo,_,O)|_],O,1) :- !.

temLixo(Lixo,[(L,_,_)|T],O,EPonto) :-
  temLixo(Lixo,T,O,EPonto).

temLixo(_,[],0,0).

%-------------------------------------------------------------

percorre_territorio(_,[]).

percorre_territorio(S,[Ponto|Territorio]) :-
  membro(Ponto,S),
  percorre_territorio(S,Territorio).


% ------------------------------------------------------------------- Pesquisa em profundidade --------------------------------------------------------------------------------------

caminho_df(Garagem, Deposito,TipoLixo,[Garagem|Solucao],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  resolvedf(Garagem,Deposito,TipoLixo,[Garagem],Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,0).

resolvedf(Deposito,Deposito,_,_,[],0,0,Acc,Acc).

resolvedf(Nodo,Deposito,TipoLixo,Historico,[ProxNodo|T],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,Acc) :-
  Acc >= 15000,
  adjacente(Nodo,ProxNodo,Distancia),
  nao(membro(ProxNodo,Historico)),
  ponto_recolha(Nodo,_,_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,PontoRecolha),
  resolvedf(ProxNodo,Deposito,TipoLixo,[ProxNodo|Historico],T,DistanciaTotalAcc,NumeroPontosRecolhaAcc,QuantidadeRecolhida,15000),
  DistanciaTotal is Distancia + DistanciaTotalAcc,
  NumeroPontosRecolha is PontoRecolha + NumeroPontosRecolhaAcc.

resolvedf(Nodo,Deposito,TipoLixo,Historico,[ProxNodo|T],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,Acc) :-
  Acc < 15000,
	adjacente(Nodo,ProxNodo,Distancia),
	nao(membro(ProxNodo,Historico)),
  ponto_recolha(Nodo,_,_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,PontoRecolha),
  Acc2 is Acc + Ocupacao,
  resolvedf(ProxNodo,Deposito,TipoLixo,[ProxNodo|Historico],T,DistanciaTotalAcc,NumeroPontosRecolhaAcc,QuantidadeRecolhida,Acc2),
  DistanciaTotal is Distancia + DistanciaTotalAcc,
  NumeroPontosRecolha is PontoRecolha + NumeroPontosRecolhaAcc.




%---------------------------------------------------Pesquisa em profundidade com obrigatoriedade de passar em certos pontos---------------------------------------------------------------

caminho_df(Garagem, Deposito,Territorio,TipoLixo,[Garagem|Solucao],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  resolvedf(Garagem,Deposito,TipoLixo,[Garagem],Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,0),
  percorre_territorio([Garagem|Solucao],Territorio).

% ------------------------------------------------------------Busca Iterativa Limitada em Profundidade----------------------------------------------------------------

caminho_df_limitada_iterativa(Garagem,Deposito,TipoLixo,Solucao,Dist,Pontos,15000) :-
  resolve_df_limitada_it(Garagem,Deposito,TipoLixo,Caminho,Dist,Pontos,Quant),
  reverse(Caminho,Solucao),
  Quant>=15000.

caminho_df_limitada_iterativa(Garagem,Deposito,TipoLixo,Solucao,Dist,Pontos,Quant) :-
  resolve_df_limitada_it(Garagem,Deposito,TipoLixo,Caminho,Dist,Pontos,Quant),
  reverse(Caminho,Solucao),
  Quant<15000.

resolve_df_limitada_it(Nodo,Nodo,_,[Nodo],0,0,0).

resolve_df_limitada_it(Garagem,Deposito,TipoLixo,[Deposito|Caminho],Dist,Pontos,Quant) :-
  resolve_df_limitada_it(Garagem,Nodo,TipoLixo,Caminho,Dist1,Pontos1,Quant1),
  adjacente(Nodo,Deposito,Distancia),
  nao(membro(Deposito, Caminho)),
  ponto_recolha(Nodo,_,_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,PontoRecolha),
  Dist is Distancia + Dist1,
  Pontos is PontoRecolha + Pontos1,
  Quant is Ocupacao + Quant1.

%-------------------------------------------------------------Busca Limitada em Profundidade ----------------------------------------------------------------------
caminho_df_limite(Garagem, Deposito,Limite,TipoLixo,[Garagem|Solucao],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  resolvedf_limite(Garagem,Deposito,Limite,TipoLixo,[Garagem],Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,0,0).

resolvedf_limite(Deposito,Deposito,_,_,_,[],0,0,Acc,Acc,_).
  resolvedf_limite(_,_,Limite,_,_,[],0,0,Acc,Acc,LimiteAcc):-
  LimiteAcc == Limite.

resolvedf_limite(Nodo,Deposito,Limite,TipoLixo,Historico,[ProxNodo|T],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,Acc,LAcc) :-
  Acc >= 15000,
  adjacente(Nodo,ProxNodo,Distancia),
  nao(membro(ProxNodo,Historico)),
  ponto_recolha(Nodo,_,_,_,ListaLixos),
  LimiteAcc is LAcc+1,
  temLixo(TipoLixo,ListaLixos,Ocupacao,PontoRecolha),
  resolvedf_limite(ProxNodo,Deposito,Limite,TipoLixo,[ProxNodo|Historico],T,DistanciaTotalAcc,NumeroPontosRecolhaAcc,QuantidadeRecolhida,15000,LimiteAcc),
  DistanciaTotal is Distancia + DistanciaTotalAcc,
  NumeroPontosRecolha is PontoRecolha + NumeroPontosRecolhaAcc.

resolvedf_limite(Nodo,Deposito,Limite,TipoLixo,Historico,[ProxNodo|T],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,Acc,LAcc) :-
  Acc < 15000,
	adjacente(Nodo,ProxNodo,Distancia),
	nao(membro(ProxNodo,Historico)),
  ponto_recolha(Nodo,_,_,_,ListaLixos),
  LimiteAcc is LAcc+1,
  temLixo(TipoLixo,ListaLixos,Ocupacao,PontoRecolha),
  Acc2 is Acc + Ocupacao,
  resolvedf_limite(ProxNodo,Deposito,Limite,TipoLixo,[ProxNodo|Historico],T,DistanciaTotalAcc,NumeroPontosRecolhaAcc,QuantidadeRecolhida,Acc2,LimiteAcc),
  DistanciaTotal is Distancia + DistanciaTotalAcc,
  NumeroPontosRecolha is PontoRecolha + NumeroPontosRecolhaAcc.






% -------------------------------------------------------------------- Pesquisa em largura --------------------------------------------------------------------------

caminho_bf(Garagem, Deposito,TipoLixo,Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  resolvebf(Deposito,TipoLixo,[([Garagem],0,0,0)],InvSolucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida),
  reverse(InvSolucao,Solucao).

resolvebf(Deposito,_,[([Deposito|Caminho],D,P,Q)|_],[Deposito|Caminho],D,P,Q).

resolvebf(Deposito,TipoLixo,[(Caminho,DAcc,PAcc,QAcc)|Caminhos],Solucao,D,P,Q) :-
  QAcc < 15000,
  extend1(TipoLixo,DAcc,PAcc,QAcc,Caminho,NovosCaminhos),
  append(Caminhos,NovosCaminhos,Caminho1),
  resolvebf(Deposito,TipoLixo,Caminho1,Solucao,D,P,Q).

resolvebf(Deposito,TipoLixo,[(Caminho,DAcc,PAcc,QAcc)|Caminhos],Solucao,D,P,Q) :-
  QAcc >= 15000,
  extend2(TipoLixo,DAcc,PAcc,15000,Caminho,NovosCaminhos),
  append(Caminhos,NovosCaminhos,Caminho1),
  resolvebf(Deposito,TipoLixo,Caminho1,Solucao,D,P,Q).

%
extend1(TipoLixo,D,P,Q,[Nodo|Caminho],NovosCaminhos) :-
  findall(([NodoAdjacente,Nodo|Caminho],DNew,PNew,QNew),

  (adjacente(Nodo,NodoAdjacente,Distancia),
  nao(membro(NodoAdjacente,[Nodo|Caminho])),
  ponto_recolha(NodoAdjacente,_,_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,Ponto),
  DNew is D+Distancia,
  PNew is P+Ponto,
  QNew is Q+Ocupacao),

  NovosCaminhos).

extend2(TipoLixo,D,P,Q,[Nodo|Caminho],NovosCaminhos) :-
  findall(([NodoAdjacente,Nodo|Caminho],DNew,P,Q),

  (adjacente(Nodo,NodoAdjacente,Distancia),
  nao(membro(NodoAdjacente,[Nodo|Caminho])),
  ponto_recolha(NodoAdjacente,_,_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,Ponto),
  DNew is D+Distancia,
  PNew is P+Ponto),

  NovosCaminhos).

%---------------------------------------------------Pesquisa em largura com obrigatoriedade de passar em certos pontos---------------------------------------------------------------

caminho_bf(Garagem, Deposito,TipoLixo,Territorio,Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  resolvebf(Deposito,TipoLixo,[([Garagem],0,0,0)],InvSolucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida),
  reverse(InvSolucao,Solucao),
  percorre_territorio(Solucao,Territorio).


% -------------------------------------------------------------------------- Gulosa -------------------------------------------------------------------------

caminho_gulosa(Garagem,Deposito,TipoLixo,Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  estimativa(Garagem,Deposito,Estimativa),
  resolve_gulosa(Deposito,TipoLixo,[[Garagem]/0/0/0/Estimativa],InvCaminho/DistanciaTotal/NumeroPontosRecolha/QuantidadeRecolhida/EstFinal),
  reverse(InvCaminho, Solucao).

resolve_gulosa(Deposito,TipoLixo,Caminhos,Caminho) :-
	obtem_melhor_g(Caminhos,Caminho),
	Caminho = [Deposito|_]/_/_/_/_.

resolve_gulosa(Deposito,TipoLixo,Caminhos,SolucaoCaminho) :-
	obtem_melhor_g(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_gulosa(Deposito,TipoLixo,MelhorCaminho, ExpandeCaminhos),
	append(OutrosCaminhos, ExpandeCaminhos, NovoCaminhos),
	resolve_gulosa(Deposito,TipoLixo,NovoCaminhos, SolucaoCaminho).

expande_gulosa(Deposito,TipoLixo,Caminho,ExpandeCaminhos) :-
	findall(NovoCaminho, adjacente_g(Deposito,TipoLixo,Caminho,NovoCaminho), ExpandeCaminhos).

adjacente_g(Deposito,TipoLixo,[Nodo|Caminho]/Dist/Pontos/Quant/_,[ProxNodo,Nodo|Caminho]/NovaDist/NovoPontos/NovaQuant/Est) :-
  Quant < 15000,
  adjacente(Nodo,ProxNodo,Distancia),
	nao(membro(ProxNodo,[Nodo|Caminho])),
  ponto_recolha(ProxNodo,(_,_),_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,Ponto),
  NovaDist is Dist+Distancia,
  NovaQuant is Quant+Ocupacao,
  NovoPontos is Pontos+Ponto,
  estimativa(ProxNodo,Deposito,Est).

adjacente_g(Deposito,TipoLixo,[Nodo|Caminho]/Dist/Pontos/Quant/_,[ProxNodo,Nodo|Caminho]/NovaDist/NovoPontos/15000/Est) :-
  Quant >= 15000,
	adjacente(Nodo,ProxNodo,Distancia),
	nao(membro(ProxNodo,[Nodo|Caminho])),
  ponto_recolha(ProxNodo,(_,_),_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,Ponto),
  NovaDist is Dist+Distancia,
  NovoPontos is Pontos+Ponto,
  estimativa(ProxNodo,Deposito,Est).

obtem_melhor_g([Caminho],Caminho) :- !.
obtem_melhor_g([Caminho1/_/_/_/Est1,Caminho2/_/_/_/Est2|Caminhos],MelhorCaminho) :-
	Est1 =< Est2, !,
	obtem_melhor_g([Caminho1/_/_/_/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g([_|Caminhos], MelhorCaminho) :-
    obtem_melhor_g(Caminhos, MelhorCaminho).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).


% ---------------------------------------------------------------------------- A* ----------------------------------------------------------------------------

caminho_estrela(Garagem,Deposito,TipoLixo,Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  estimativa(Garagem,Deposito,Estimativa),
  resolve_estrela(Deposito,TipoLixo,[[Garagem]/0/0/0/Estimativa],InvCaminho/DistanciaTotal/NumeroPontosRecolha/QuantidadeRecolhida/EstFinal),
  reverse(InvCaminho, Solucao).

resolve_estrela(Deposito,TipoLixo,Caminhos,Caminho) :-
	obtem_melhor_e(Caminhos,Caminho),
	Caminho = [Deposito|_]/_/_/_/_.

resolve_estrela(Deposito,TipoLixo,Caminhos,SolucaoCaminho) :-
	obtem_melhor_e(Caminhos,MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_estrela(Deposito,TipoLixo,MelhorCaminho, ExpandeCaminhos),
	append(OutrosCaminhos, ExpandeCaminhos, NovoCaminhos),
	resolve_estrela(Deposito,TipoLixo,NovoCaminhos, SolucaoCaminho).

expande_estrela(Deposito,TipoLixo,Caminho,ExpandeCaminhos) :-
	findall(NovoCaminho, adjacente_e(Deposito,TipoLixo,Caminho,NovoCaminho), ExpandeCaminhos).

adjacente_e(Deposito,TipoLixo,[Nodo|Caminho]/Dist/Pontos/Quant/_,[ProxNodo,Nodo|Caminho]/NovaDist/NovoPontos/NovaQuant/Est) :-
  Quant < 15000,
	adjacente(Nodo,ProxNodo,Distancia),
	nao(membro(ProxNodo,[Nodo|Caminho])),
  ponto_recolha(ProxNodo,(_,_),_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,Ponto),
  NovaDist is Dist+Distancia,
  NovaQuant is Quant+Ocupacao,
  NovoPontos is Pontos+Ponto,
  estimativa(ProxNodo,Deposito,Est).

adjacente_e(Deposito,TipoLixo,[Nodo|Caminho]/Dist/Pontos/Quant/_,[ProxNodo,Nodo|Caminho]/NovaDist/NovoPontos/15000/Est) :-
  Quant >= 15000,
	adjacente(Nodo,ProxNodo,Distancia),
	nao(membro(ProxNodo,[Nodo|Caminho])),
  ponto_recolha(ProxNodo,(_,_),_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,Ponto),
  NovaDist is Dist+Distancia,
  NovoPontos is Pontos+Ponto,
  estimativa(ProxNodo,Deposito,Est).

obtem_melhor_e([Caminho],Caminho) :- !.

obtem_melhor_e([Caminho1/Dist1/_/_/Est1,Caminho2/Dist2/_/_/Est2|Caminhos],MelhorCaminho) :-
	Est1+Dist1 =< Est2+Dist2, !,
	obtem_melhor_e([Caminho1/Dist1/_/_/Est1|Caminhos], MelhorCaminho).

obtem_melhor_e([_|Caminhos], MelhorCaminho) :-
  obtem_melhor_e(Caminhos, MelhorCaminho).


% --------------------------------------------------------------------------------------------------------------------------------------------

nao( Questao ) :-
    Questao, !, fail.
nao( _Questao ).

membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).
