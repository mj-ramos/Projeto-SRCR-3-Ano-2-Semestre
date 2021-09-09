%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - 2021

% TP INDIVIDUAL
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Maria Ramos, a89541
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

:- consult('grafo.pl').
:- consult('TP_individual.pl').

% ------------------------------------------------------- Predicados auxiliares ---------------------------------------------------------

maxC([(S,C)],(S,C)).
maxC([(_,C)|T],(S1,C1)) :- maxC(T,(S1,C1)), C1>C,!.
maxC([(S,C)|_],(S,C)).

minDP([(S,D,P)],(S,D,P)).

minDP([(_,D,P)|T],(S1,D1,P1)) :-
  minDP(T,(S1,D1,P1)),
  M1 is P1/D1,
  M is P/D,
  M1<M, !.

minDP([(S,D,P)|_],(S,D,P)).

minC([(S,C)],(S,C)).
minC([(_,C)|T],(S1,C1)) :- minC(T,(S1,C1)), C1<C,!.
minC([(S,C)|_],(S,C)).



% -----------------------------Procura todos os caminhos com a pesquisa em profundidade e seleciona um com base numa métrica ----------------------------------

melhorCaminhoMinDistanciaDF(Garagem,Destino,TipoLixo,Distancia, MelhorCaminho) :-
  statistics(runtime,[Start|_]),
  findall((Solucao,Distancia),caminho_df(Garagem,Destino,TipoLixo,Solucao,Distancia,_,_), Solucoes),
  minC(Solucoes,(MelhorCaminho,Distancia)),
  statistics(runtime,[Stop|_]),
  Runtime is Stop-Start,
  write("Tempo: "),write(Runtime).

melhorCaminhoMaxPontosRecolhaDF(Garagem,Destino,TipoLixo,PontosRecolha,MelhorCaminho) :-
  statistics(runtime,[Start|_]),
  findall((Solucao,PontosRecolha),caminho_df(Garagem,Destino,TipoLixo,Solucao,_,PontosRecolha,_), Solucoes),
  maxC(Solucoes,(MelhorCaminho,PontosRecolha)),
  statistics(runtime,[Stop|_]),
  Runtime is Stop-Start,
  write("Tempo: "),write(Runtime).


melhorCaminhoMaxRecolhaDF(Garagem,Destino,TipoLixo,QuantidadeRecolhida, MelhorCaminho) :-
  findall((Solucao,QuantidadeRecolhida),caminho_df(Garagem,Destino,TipoLixo,Solucao,_,_,QuantidadeRecolhida), Solucoes),
  maxC(Solucoes,(MelhorCaminho,QuantidadeRecolhida)).

melhorCaminhoMaxRecolhaDFL(Garagem,Destino,TipoLixo,Limite,QuantidadeRecolhida, MelhorCaminho) :-
  findall((Solucao,QuantidadeRecolhida),caminho_df_limite(Garagem,Destino,Limite,TipoLixo,Solucao,_,_,QuantidadeRecolhida), Solucoes),
  maxC(Solucoes,(MelhorCaminho,QuantidadeRecolhida)).

melhorCaminhoMinDisEntrePontosDF(Garagem,Destino,TipoLixo,DistPontos,MelhorCaminho) :-
  findall((Solucao,Distancia,PontosRecolha),caminho_df(Garagem,Destino,TipoLixo,Solucao,Distancia,PontosRecolha,_), Solucoes),
  minDP(Solucoes,(MelhorCaminho,D,P)),
  P=\=0, !,
  DistPontos is D/P.

melhorCaminhoMinDisEntrePontosDFL(Garagem,Destino,TipoLixo,Limite,DistPontos,MelhorCaminho) :-
  findall((Solucao,Distancia,PontosRecolha),caminho_df_limite(Garagem,Destino,Limite,TipoLixo,Solucao,Distancia,PontosRecolha,_), Solucoes),
  minDP(Solucoes,(MelhorCaminho,D,P)),
  P=\=0, !,
  DistPontos is D/P.


%---------------------------------------Compara os algoritmos com base no número de pontos de recolha------------------------------------------------------------

comparaProcuraMaisRecolha(Garagem,Destino,TipoLixo,Limite) :-
  write("Procura em Profundidade com findall\n"),
  statistics(runtime,[Start|_]),
  melhorCaminhoMaxRecolhaDF(Garagem,Destino,TipoLixo,QuantidadeDF,MelhorCaminhoDF),
  statistics(runtime,[Stop|_]),
  write("Caminho: "),write(MelhorCaminhoDF),
  write("\nQuantidade: "),write(QuantidadeDF),
  Runtime is Stop-Start,
  write("\nTempo: "),write(Runtime),

  write("\n\nProcura em Profundidade com limite com findall\n"),
  statistics(runtime,[Start1|_]),
  melhorCaminhoMaxRecolhaDFL(Garagem,Destino,TipoLixo,Limite,QuantidadeDFL,MelhorCaminhoDFL),
  statistics(runtime,[Stop1|_]),
  write("Caminho: "),write(MelhorCaminhoDFL),
  write("\nQuantidade: "),write(QuantidadeDFL),
  Runtime1 is Stop1-Start1,
  write("\nTempo: "),write(Runtime1),

  write("\n\nProcura em Largura\n"),
  statistics(runtime,[Start2|_]),
  caminho_bf(Garagem,Destino,TipoLixo,CaminhoBF,_,_,QuantidadeBF),
  statistics(runtime,[Stop2|_]),
  write("Caminho: "),write(CaminhoBF),
  write("\nQuantidade: "),write(QuantidadeBF),
  Runtime2 is Stop2-Start2,
  write("\nTempo: "),write(Runtime2),

  write("\n\nGulosa\n"),
  statistics(runtime,[Start3|_]),
  caminho_gulosa(Garagem,Destino,TipoLixo,CaminhoG,_,_,QuantidadeG),
  statistics(runtime,[Stop3|_]),
  write("Caminho: "),write(CaminhoG),
  write("\nQuantidade: "),write(QuantidadeG),
  Runtime3 is Stop3-Start3,
  write("\nTempo: "),write(Runtime3),

  write("\n\nA*\n"),
  statistics(runtime,[Start4|_]),
  caminho_bf(Garagem,Destino,TipoLixo,CaminhoA,_,_,QuantidadeA),
  statistics(runtime,[Stop4|_]),
  write("Caminho:"),write(CaminhoA),
  write("\nQuantidade:"),write(QuantidadeA),
  Runtime4 is Stop4-Start4,
  write("\nTempo: "),write(Runtime4).



  %---------------------------------------Compara os algoritmos com base na distância média entre pontos de recolha------------------------------------------------------------


comparaProcuraMenosDistPontos(Garagem,Destino,TipoLixo,Limite) :-
  write("Procura em Profundidade com findall\n"),
  statistics(runtime,[Start|_]),
  melhorCaminhoMinDisEntrePontosDF(Garagem,Destino,TipoLixo,MinDistDF,MelhorCaminhoDF),
  write("Caminho: "),write(MelhorCaminhoDF),
  write("\nDistancia media entre pontos: "),write(MinDistDF),
  statistics(runtime,[Stop|_]),
  Runtime is Stop-Start,
  write("\nTempo: "),write(Runtime),

  write("\n\nProcura em Profundidade com limite com findall\n"),
  statistics(runtime,[Start1|_]),
  melhorCaminhoMinDisEntrePontosDFL(Garagem,Destino,TipoLixo,Limite,MinDistDFL,MelhorCaminhoDFL),
  write("Caminho: "),write(MelhorCaminhoDFL),
  write("\nDistancia media entre pontos: "),write(MinDistDFL),
  statistics(runtime,[Stop1|_]),
  Runtime1 is Stop1-Start1,
  write("\nTempo: "),write(Runtime1),

  write("\n\nProcura em Largura\n"),
  statistics(runtime,[Start2|_]),
  caminho_bf(Garagem,Destino,TipoLixo,CaminhoBF,DistBF,PontosBF,_),
  statistics(runtime,[Stop2|_]),
  write("Caminho: "),write(CaminhoBF),
  DistMediaBF is DistBF/PontosBF,
  DistMediaBF =\= 0,
  write("\nDistancia media entre pontos: "),write(DistMediaBF),
  statistics(runtime,[Stop2|_]),
  Runtime2 is Stop2-Start2,
  write("\nTempo: "),write(Runtime2),

  write("\n\nGulosa\n"),
  statistics(runtime,[Start3|_]),
  caminho_gulosa(Garagem,Destino,TipoLixo,CaminhoG,DistG,PontosG,_),
  statistics(runtime,[Stop3|_]),
  write("Caminho: "),write(CaminhoG),
  DistMediaG is DistG/PontosG,
  DistMediaG =\= 0,
  write("\nDistancia media entre pontos: "),write(DistMediaG),
  Runtime3 is Stop3-Start3,
  write("\nTempo: "),write(Runtime3),

  write("\n\nA*\n"),
  statistics(runtime,[Start4|_]),
  caminho_bf(Garagem,Destino,TipoLixo,CaminhoA,DistA,PontosA,_),
  statistics(runtime,[Stop4|_]),
  write("Caminho:"),write(CaminhoA),
  DistMediaA is DistA/PontosA,
  DistMediaA =\= 0,
  write("\nDistancia media entre pontos: "),write(DistMediaA),
  Runtime4 is Stop4-Start4,
  write("\nTempo: "),write(Runtime4).




%-------------------------------------------------------------------------Percurso mais eficiente----------------------------------------------------------------------------------------------

caminho_df_ef(Garagem, Deposito,TipoLixo,[Garagem|Solucao],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida):-
  resolvedf_ef(Garagem,Deposito,TipoLixo,[Garagem],Solucao,DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,0).

resolvedf_ef(_,_,_,_,[],0,0,15000,Quant) :-
  Quant >= 15000, !.
resolvedf_ef(Deposito,Deposito,_,_,[],0,0,Acc,Acc).

resolvedf_ef(Nodo,Deposito,TipoLixo,Historico,[ProxNodo|T],DistanciaTotal,NumeroPontosRecolha,QuantidadeRecolhida,Acc) :-
  Acc < 15000,
	adjacente(Nodo,ProxNodo,Distancia),
	nao(membro(ProxNodo,Historico)),
  ponto_recolha(Nodo,_,_,_,ListaLixos),
  temLixo(TipoLixo,ListaLixos,Ocupacao,PontoRecolha),
  Acc2 is Acc + Ocupacao,
  resolvedf_ef(ProxNodo,Deposito,TipoLixo,[ProxNodo|Historico],T,DistanciaTotalAcc,NumeroPontosRecolhaAcc,QuantidadeRecolhida,Acc2),
  DistanciaTotal is Distancia + DistanciaTotalAcc,
  NumeroPontosRecolha is PontoRecolha + NumeroPontosRecolhaAcc.

caminho_ef(Garagem,Deposito,TipoLixo,Quant,MelhorCaminho) :-
  findall((Solucao,QuantidadeRecolhida),caminho_df_ef(Garagem,Destino,TipoLixo,Solucao,_,_,QuantidadeRecolhida), Solucoes),
  maxC(Solucoes,(MelhorCaminho,Quant)).
%-------------------------------------------------------------------------------------------------------------------------------------------

caminho_mais_eficiente(Garagem,Deposito,TipoLixo,Solucao,Quant) :-
    caminho_ef(Garagem,Deposito,TipoLixo,Quant,SolDF),
    reverse(SolDF,[Fim|Inv]),
    caminho_gulosa(Fim,Deposito,TipoLixo,SolGulosa,_,_,QuantG),
    append(SolDF,SolGulosa,Solucao), !.
