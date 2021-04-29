Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(MVotacao_i))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(MVotacao_i))==(Machine(MVotacao));
  Level(Implementation(MVotacao_i))==(1);
  Upper_Level(Implementation(MVotacao_i))==(Machine(MVotacao))
END
&
THEORY LoadedStructureX IS
  Implementation(MVotacao_i)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(MVotacao_i))==(MVotacao_Ctx)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(MVotacao_i))==(?);
  Inherited_List_Includes(Implementation(MVotacao_i))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(MVotacao_i))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(MVotacao_i))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(MVotacao_i))==(?);
  Context_List_Variables(Implementation(MVotacao_i))==(?);
  Abstract_List_Variables(Implementation(MVotacao_i))==(registro,votos,local,candidatos,eleitores,estado);
  Local_List_Variables(Implementation(MVotacao_i))==(?);
  List_Variables(Implementation(MVotacao_i))==(?);
  External_List_Variables(Implementation(MVotacao_i))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(MVotacao_i))==(?);
  Abstract_List_VisibleVariables(Implementation(MVotacao_i))==(?);
  External_List_VisibleVariables(Implementation(MVotacao_i))==(?);
  Expanded_List_VisibleVariables(Implementation(MVotacao_i))==(?);
  List_VisibleVariables(Implementation(MVotacao_i))==(registro_i,votos_i,local_i,candidatos_i,eleitores_i,estado_i);
  Internal_List_VisibleVariables(Implementation(MVotacao_i))==(registro_i,votos_i,local_i,candidatos_i,eleitores_i,estado_i)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(MVotacao_i))==(btrue);
  Expanded_List_Invariant(Implementation(MVotacao_i))==(btrue);
  Abstract_List_Invariant(Implementation(MVotacao_i))==(estado: ESTADO & eleitores: FIN(CPFs) & candidatos: FIN(CPFs) & local: eleitores +-> ZONAS & votos: candidatos +-> NAT & registro: eleitores +-> BOOL & !aa.(aa: eleitores => aa/=indefinido) & !ee.(ee: candidatos => ee/=indefinido) & !ii.(ii: candidatos => ii: eleitores) & !oo.(oo: ran(registro) & estado = preparacao => oo = FALSE) & !uu.(uu: ran(votos) & estado = preparacao => uu = 0));
  Context_List_Invariant(Implementation(MVotacao_i))==(btrue);
  List_Invariant(Implementation(MVotacao_i))==(estado_i: ESTADO & eleitores_i: CPFs --> BOOL & candidatos_i: CPFs --> BOOL & local_i: CPFs --> (ZONAS --> BOOL) & votos_i: CPFs --> NAT & registro_i: CPFs --> BOOL)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Implementation(MVotacao_i))==(btrue);
  Abstract_List_Assertions(Implementation(MVotacao_i))==(btrue);
  Context_List_Assertions(Implementation(MVotacao_i))==(btrue);
  List_Assertions(Implementation(MVotacao_i))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Implementation(MVotacao_i))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Implementation(MVotacao_i))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(MVotacao_i))==(estado_i:=preparacao;eleitores_i:=CPFs*{FALSE};candidatos_i:=CPFs*{FALSE};local_i:=CPFs*{ZONAS*{FALSE}};votos_i:=CPFs*{0};registro_i:=CPFs*{FALSE});
  Context_List_Initialisation(Implementation(MVotacao_i))==(skip);
  List_Initialisation(Implementation(MVotacao_i))==(estado_i:=preparacao;eleitores_i:=CPFs*{FALSE};candidatos_i:=CPFs*{FALSE};local_i:=CPFs*{ZONAS*{FALSE}};votos_i:=CPFs*{0};registro_i:=CPFs*{FALSE})
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(MVotacao_i))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(MVotacao_i),Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(MVotacao_i))==(btrue);
  List_Context_Constraints(Implementation(MVotacao_i))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(MVotacao_i))==(adicionar_eleitor,remover_eleitor,mudar_local,adicionar_candidato,remover_candidato,iniciar_votacao,votar,concluir_votacao,apurar_vencedor);
  List_Operations(Implementation(MVotacao_i))==(adicionar_eleitor,remover_eleitor,mudar_local,adicionar_candidato,remover_candidato,iniciar_votacao,votar,concluir_votacao,apurar_vencedor)
END
&
THEORY ListInputX IS
  List_Input(Implementation(MVotacao_i),adicionar_eleitor)==(cpf,zona);
  List_Input(Implementation(MVotacao_i),remover_eleitor)==(cpf);
  List_Input(Implementation(MVotacao_i),mudar_local)==(cpf,zona);
  List_Input(Implementation(MVotacao_i),adicionar_candidato)==(cpf);
  List_Input(Implementation(MVotacao_i),remover_candidato)==(cpf);
  List_Input(Implementation(MVotacao_i),iniciar_votacao)==(?);
  List_Input(Implementation(MVotacao_i),votar)==(cpf,zona,cpf_candidato);
  List_Input(Implementation(MVotacao_i),concluir_votacao)==(?);
  List_Input(Implementation(MVotacao_i),apurar_vencedor)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(MVotacao_i),adicionar_eleitor)==(?);
  List_Output(Implementation(MVotacao_i),remover_eleitor)==(?);
  List_Output(Implementation(MVotacao_i),mudar_local)==(?);
  List_Output(Implementation(MVotacao_i),adicionar_candidato)==(?);
  List_Output(Implementation(MVotacao_i),remover_candidato)==(?);
  List_Output(Implementation(MVotacao_i),iniciar_votacao)==(?);
  List_Output(Implementation(MVotacao_i),votar)==(?);
  List_Output(Implementation(MVotacao_i),concluir_votacao)==(?);
  List_Output(Implementation(MVotacao_i),apurar_vencedor)==(resultado)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(MVotacao_i),adicionar_eleitor)==(adicionar_eleitor(cpf,zona));
  List_Header(Implementation(MVotacao_i),remover_eleitor)==(remover_eleitor(cpf));
  List_Header(Implementation(MVotacao_i),mudar_local)==(mudar_local(cpf,zona));
  List_Header(Implementation(MVotacao_i),adicionar_candidato)==(adicionar_candidato(cpf));
  List_Header(Implementation(MVotacao_i),remover_candidato)==(remover_candidato(cpf));
  List_Header(Implementation(MVotacao_i),iniciar_votacao)==(iniciar_votacao);
  List_Header(Implementation(MVotacao_i),votar)==(votar(cpf,zona,cpf_candidato));
  List_Header(Implementation(MVotacao_i),concluir_votacao)==(concluir_votacao);
  List_Header(Implementation(MVotacao_i),apurar_vencedor)==(resultado <-- apurar_vencedor)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(MVotacao_i),adicionar_eleitor)==(btrue);
  List_Precondition(Implementation(MVotacao_i),adicionar_eleitor)==(estado = preparacao & cpf: CPFs & cpf/:eleitores & zona: ZONAS & cpf/=indefinido);
  Own_Precondition(Implementation(MVotacao_i),remover_eleitor)==(btrue);
  List_Precondition(Implementation(MVotacao_i),remover_eleitor)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido);
  Own_Precondition(Implementation(MVotacao_i),mudar_local)==(btrue);
  List_Precondition(Implementation(MVotacao_i),mudar_local)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf: dom(local) & zona: ZONAS & zona/=local(cpf) & cpf/=indefinido);
  Own_Precondition(Implementation(MVotacao_i),adicionar_candidato)==(btrue);
  List_Precondition(Implementation(MVotacao_i),adicionar_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf/:candidatos);
  Own_Precondition(Implementation(MVotacao_i),remover_candidato)==(btrue);
  List_Precondition(Implementation(MVotacao_i),remover_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf: candidatos);
  Own_Precondition(Implementation(MVotacao_i),iniciar_votacao)==(btrue);
  List_Precondition(Implementation(MVotacao_i),iniciar_votacao)==(estado = preparacao & candidatos/={});
  Own_Precondition(Implementation(MVotacao_i),votar)==(btrue);
  List_Precondition(Implementation(MVotacao_i),votar)==(estado = votacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & zona: ZONAS & cpf: dom(local) & cpf: dom(registro) & zona = local(cpf) & cpf_candidato: CPFs & cpf_candidato: candidatos\/{indefinido} & registro(cpf) = FALSE);
  Own_Precondition(Implementation(MVotacao_i),concluir_votacao)==(btrue);
  List_Precondition(Implementation(MVotacao_i),concluir_votacao)==(estado = votacao);
  Own_Precondition(Implementation(MVotacao_i),apurar_vencedor)==(btrue);
  List_Precondition(Implementation(MVotacao_i),apurar_vencedor)==(estado = apuracao)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(MVotacao_i),apurar_vencedor)==(estado = apuracao | @(cand_1,cand_2,cand_3).((cpf1: dom(votos_i) | cand_1:=votos_i(cpf1));(cpf2: dom(votos_i) | cand_2:=votos_i(cpf2));(cpf3: dom(votos_i) | cand_3:=votos_i(cpf3));(cand_1>cand_2 & cand_1>cand_3 ==> resultado:=cpf1 [] not(cand_1>cand_2 & cand_1>cand_3) ==> (cand_2>cand_1 & cand_2>cand_3 ==> resultado:=cpf2 [] not(cand_2>cand_1 & cand_2>cand_3) ==> (cand_3>cand_1 & cand_3>cand_2 ==> resultado:=cpf3 [] not(cand_3>cand_1 & cand_3>cand_2) ==> resultado:=indefinido)))));
  Expanded_List_Substitution(Implementation(MVotacao_i),concluir_votacao)==(estado = votacao | estado_i = votacao ==> estado_i:=apuracao [] not(estado_i = votacao) ==> skip);
  Expanded_List_Substitution(Implementation(MVotacao_i),votar)==(estado = votacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & zona: ZONAS & cpf: dom(local) & cpf: dom(registro) & zona = local(cpf) & cpf_candidato: CPFs & cpf_candidato: candidatos\/{indefinido} & registro(cpf) = FALSE | @(ee,ii,oo,uu).((cpf: dom(eleitores_i) | ii:=eleitores_i(cpf));(cpf_candidato: dom(candidatos_i) | oo:=candidatos_i(cpf_candidato));(cpf: dom(registro_i) | uu:=registro_i(cpf));(cpf: dom(local_i) & zona: dom(local_i(cpf)) | ee:=local_i(cpf)(zona));(ii = TRUE & ee = TRUE & uu = FALSE ==> (oo = TRUE ==> (cpf_candidato: dom(votos_i) & votos_i(cpf_candidato)+1: INT & votos_i(cpf_candidato): INT & 1: INT | votos_i:=votos_i<+{cpf_candidato|->votos_i(cpf_candidato)+1}) [] not(oo = TRUE) ==> skip;(cpf: dom(registro_i) | registro_i:=registro_i<+{cpf|->TRUE})) [] not(ii = TRUE & ee = TRUE & uu = FALSE) ==> skip)));
  Expanded_List_Substitution(Implementation(MVotacao_i),iniciar_votacao)==(estado = preparacao & candidatos/={} | @(eleitor_1,eleitor_2,eleitor_3).((cpf1: dom(candidatos_i) | eleitor_1:=candidatos_i(cpf1));(cpf2: dom(candidatos_i) | eleitor_2:=candidatos_i(cpf2));(cpf3: dom(candidatos_i) | eleitor_3:=candidatos_i(cpf3));(eleitor_1 = TRUE or eleitor_2 = TRUE or eleitor_3 = TRUE ==> estado_i:=votacao [] not(eleitor_1 = TRUE or eleitor_2 = TRUE or eleitor_3 = TRUE) ==> skip)));
  Expanded_List_Substitution(Implementation(MVotacao_i),remover_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf: candidatos | @(ii,oo).((cpf: dom(eleitores_i) | ii:=eleitores_i(cpf));(cpf: dom(candidatos_i) | oo:=candidatos_i(cpf));(ii = TRUE & oo = TRUE ==> ((cpf: dom(votos_i) & 0: INT | votos_i:=votos_i<+{cpf|->0});(cpf: dom(candidatos_i) | candidatos_i:=candidatos_i<+{cpf|->FALSE})) [] not(ii = TRUE & oo = TRUE) ==> skip)));
  Expanded_List_Substitution(Implementation(MVotacao_i),adicionar_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf/:candidatos | @(ii,oo).((cpf: dom(eleitores_i) | ii:=eleitores_i(cpf));(cpf: dom(candidatos_i) | oo:=candidatos_i(cpf));(ii = TRUE & oo = FALSE ==> ((cpf: dom(candidatos_i) | candidatos_i:=candidatos_i<+{cpf|->TRUE});(cpf: dom(votos_i) & 0: INT | votos_i:=votos_i<+{cpf|->0})) [] not(ii = TRUE & oo = FALSE) ==> skip)));
  Expanded_List_Substitution(Implementation(MVotacao_i),mudar_local)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf: dom(local) & zona: ZONAS & zona/=local(cpf) & cpf/=indefinido | @ii.((cpf: dom(eleitores_i) | ii:=eleitores_i(cpf));(ii = TRUE ==> ((cpf: dom(local_i) | local_i:=local_i<+{cpf|->ZONAS*{FALSE}});(cpf: dom(local_i) & zona: dom(local_i(cpf)) | local_i:=local_i<+{cpf|->(local_i(cpf)<+{zona|->TRUE})})) [] not(ii = TRUE) ==> skip)));
  Expanded_List_Substitution(Implementation(MVotacao_i),remover_eleitor)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido | @(ii,oo).((cpf: dom(eleitores_i) | oo:=eleitores_i(cpf));(cpf: dom(candidatos_i) | ii:=candidatos_i(cpf));(oo = TRUE ==> (ii = TRUE ==> ((cpf: dom(votos_i) & 0: INT | votos_i:=votos_i<+{cpf|->0});(cpf: dom(candidatos_i) | candidatos_i:=candidatos_i<+{cpf|->FALSE})) [] not(ii = TRUE) ==> skip;(cpf: dom(local_i) | local_i:=local_i<+{cpf|->ZONAS*{FALSE}});(cpf: dom(registro_i) | registro_i:=registro_i<+{cpf|->FALSE});(cpf: dom(eleitores_i) | eleitores_i:=eleitores_i<+{cpf|->FALSE})) [] not(oo = TRUE) ==> skip)));
  Expanded_List_Substitution(Implementation(MVotacao_i),adicionar_eleitor)==(estado = preparacao & cpf: CPFs & cpf/:eleitores & zona: ZONAS & cpf/=indefinido | @ii.((cpf: dom(eleitores_i) | ii:=eleitores_i(cpf));(ii = FALSE ==> ((cpf: dom(eleitores_i) | eleitores_i:=eleitores_i<+{cpf|->TRUE});(cpf: dom(local_i) & zona: dom(local_i(cpf)) | local_i:=local_i<+{cpf|->(local_i(cpf)<+{zona|->TRUE})});(cpf: dom(registro_i) | registro_i:=registro_i<+{cpf|->FALSE})) [] not(ii = FALSE) ==> skip)));
  List_Substitution(Implementation(MVotacao_i),adicionar_eleitor)==(VAR ii IN ii:=eleitores_i(cpf);IF ii = FALSE THEN eleitores_i(cpf):=TRUE;local_i(cpf)(zona):=TRUE;registro_i(cpf):=FALSE END END);
  List_Substitution(Implementation(MVotacao_i),remover_eleitor)==(VAR ii,oo IN oo:=eleitores_i(cpf);ii:=candidatos_i(cpf);IF oo = TRUE THEN IF ii = TRUE THEN votos_i(cpf):=0;candidatos_i(cpf):=FALSE END;local_i(cpf):=ZONAS*{FALSE};registro_i(cpf):=FALSE;eleitores_i(cpf):=FALSE END END);
  List_Substitution(Implementation(MVotacao_i),mudar_local)==(VAR ii IN ii:=eleitores_i(cpf);IF ii = TRUE THEN local_i(cpf):=ZONAS*{FALSE};local_i(cpf)(zona):=TRUE END END);
  List_Substitution(Implementation(MVotacao_i),adicionar_candidato)==(VAR ii,oo IN ii:=eleitores_i(cpf);oo:=candidatos_i(cpf);IF ii = TRUE & oo = FALSE THEN candidatos_i(cpf):=TRUE;votos_i(cpf):=0 END END);
  List_Substitution(Implementation(MVotacao_i),remover_candidato)==(VAR ii,oo IN ii:=eleitores_i(cpf);oo:=candidatos_i(cpf);IF ii = TRUE & oo = TRUE THEN votos_i(cpf):=0;candidatos_i(cpf):=FALSE END END);
  List_Substitution(Implementation(MVotacao_i),iniciar_votacao)==(VAR eleitor_1,eleitor_2,eleitor_3 IN eleitor_1:=candidatos_i(cpf1);eleitor_2:=candidatos_i(cpf2);eleitor_3:=candidatos_i(cpf3);IF eleitor_1 = TRUE or eleitor_2 = TRUE or eleitor_3 = TRUE THEN estado_i:=votacao END END);
  List_Substitution(Implementation(MVotacao_i),votar)==(VAR ee,ii,oo,uu IN ii:=eleitores_i(cpf);oo:=candidatos_i(cpf_candidato);uu:=registro_i(cpf);ee:=local_i(cpf)(zona);IF ii = TRUE & ee = TRUE & uu = FALSE THEN IF oo = TRUE THEN votos_i(cpf_candidato):=votos_i(cpf_candidato)+1 END;registro_i(cpf):=TRUE END END);
  List_Substitution(Implementation(MVotacao_i),concluir_votacao)==(IF estado_i = votacao THEN estado_i:=apuracao END);
  List_Substitution(Implementation(MVotacao_i),apurar_vencedor)==(VAR cand_1,cand_2,cand_3 IN cand_1:=votos_i(cpf1);cand_2:=votos_i(cpf2);cand_3:=votos_i(cpf3);IF cand_1>cand_2 & cand_1>cand_3 THEN resultado:=cpf1 ELSE IF cand_2>cand_1 & cand_2>cand_3 THEN resultado:=cpf2 ELSE IF cand_3>cand_1 & cand_3>cand_2 THEN resultado:=cpf3 ELSE resultado:=indefinido END END END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(MVotacao_i))==(?);
  Inherited_List_Constants(Implementation(MVotacao_i))==(?);
  List_Constants(Implementation(MVotacao_i))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(MVotacao_i),ZONAS)==({zona1,zona2});
  Context_List_Enumerated(Implementation(MVotacao_i))==(ESTADO,CPFs,ZONAS);
  Context_List_Defered(Implementation(MVotacao_i))==(?);
  Context_List_Sets(Implementation(MVotacao_i))==(ESTADO,CPFs,ZONAS);
  List_Own_Enumerated(Implementation(MVotacao_i))==(?);
  List_Valuable_Sets(Implementation(MVotacao_i))==(?);
  Inherited_List_Enumerated(Implementation(MVotacao_i))==(?);
  Inherited_List_Defered(Implementation(MVotacao_i))==(?);
  Inherited_List_Sets(Implementation(MVotacao_i))==(?);
  List_Enumerated(Implementation(MVotacao_i))==(?);
  List_Defered(Implementation(MVotacao_i))==(?);
  List_Sets(Implementation(MVotacao_i))==(?);
  Set_Definition(Implementation(MVotacao_i),CPFs)==({cpf1,cpf2,cpf3,indefinido});
  Set_Definition(Implementation(MVotacao_i),ESTADO)==({preparacao,votacao,apuracao})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(MVotacao_i))==(?);
  Expanded_List_HiddenConstants(Implementation(MVotacao_i))==(?);
  List_HiddenConstants(Implementation(MVotacao_i))==(?);
  External_List_HiddenConstants(Implementation(MVotacao_i))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(MVotacao_i))==(btrue);
  Context_List_Properties(Implementation(MVotacao_i))==(ESTADO: FIN(INTEGER) & not(ESTADO = {}) & CPFs: FIN(INTEGER) & not(CPFs = {}) & ZONAS: FIN(INTEGER) & not(ZONAS = {}));
  Inherited_List_Properties(Implementation(MVotacao_i))==(btrue);
  List_Properties(Implementation(MVotacao_i))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(MVotacao_i))==(aa: aa);
  List_Values(Implementation(MVotacao_i))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(MVotacao_i),Machine(MVotacao_Ctx))==(?);
  Seen_Context_List_Enumerated(Implementation(MVotacao_i))==(?);
  Seen_Context_List_Invariant(Implementation(MVotacao_i))==(btrue);
  Seen_Context_List_Assertions(Implementation(MVotacao_i))==(btrue);
  Seen_Context_List_Properties(Implementation(MVotacao_i))==(btrue);
  Seen_List_Constraints(Implementation(MVotacao_i))==(btrue);
  Seen_List_Operations(Implementation(MVotacao_i),Machine(MVotacao_Ctx))==(?);
  Seen_Expanded_List_Invariant(Implementation(MVotacao_i),Machine(MVotacao_Ctx))==(btrue)
END
&
THEORY ListIncludedOperationsX END
&
THEORY InheritedEnvX IS
  VisibleVariables(Implementation(MVotacao_i))==(Type(estado_i) == Mvv(etype(ESTADO,?,?));Type(eleitores_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(BOOL,0,1)));Type(candidatos_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(BOOL,0,1)));Type(local_i) == Mvv(SetOf(etype(CPFs,0,3)*SetOf(etype(ZONAS,0,1)*btype(BOOL,0,1))));Type(votos_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(INTEGER,0,MAXINT)));Type(registro_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(BOOL,0,1))));
  Operations(Implementation(MVotacao_i))==(Type(apurar_vencedor) == Cst(etype(CPFs,?,?),No_type);Type(concluir_votacao) == Cst(No_type,No_type);Type(votar) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?)*etype(CPFs,?,?));Type(iniciar_votacao) == Cst(No_type,No_type);Type(remover_candidato) == Cst(No_type,etype(CPFs,?,?));Type(adicionar_candidato) == Cst(No_type,etype(CPFs,?,?));Type(mudar_local) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?));Type(remover_eleitor) == Cst(No_type,etype(CPFs,?,?));Type(adicionar_eleitor) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?)))
END
&
THEORY ListVisibleStaticX IS
  List_Constants_Env(Implementation(MVotacao_i),Machine(MVotacao_Ctx))==(Type(preparacao) == Cst(etype(ESTADO,0,2));Type(votacao) == Cst(etype(ESTADO,0,2));Type(apuracao) == Cst(etype(ESTADO,0,2));Type(cpf1) == Cst(etype(CPFs,0,3));Type(cpf2) == Cst(etype(CPFs,0,3));Type(cpf3) == Cst(etype(CPFs,0,3));Type(indefinido) == Cst(etype(CPFs,0,3));Type(zona1) == Cst(etype(ZONAS,0,1));Type(zona2) == Cst(etype(ZONAS,0,1)));
  Enumerate_Definition(Implementation(MVotacao_i),Machine(MVotacao_Ctx),ZONAS)==({zona1,zona2});
  Enumerate_Definition(Implementation(MVotacao_i),Machine(MVotacao_Ctx),CPFs)==({cpf1,cpf2,cpf3,indefinido});
  Enumerate_Definition(Implementation(MVotacao_i),Machine(MVotacao_Ctx),ESTADO)==({preparacao,votacao,apuracao})
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(MVotacao_i)) == (? | ? | ? | ? | adicionar_eleitor,remover_eleitor,mudar_local,adicionar_candidato,remover_candidato,iniciar_votacao,votar,concluir_votacao,apurar_vencedor | ? | seen(Machine(MVotacao_Ctx)) | ? | MVotacao_i);
  List_Of_HiddenCst_Ids(Implementation(MVotacao_i)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(MVotacao_i)) == (?);
  List_Of_VisibleVar_Ids(Implementation(MVotacao_i)) == (registro_i,votos_i,local_i,candidatos_i,eleitores_i,estado_i | ?);
  List_Of_Ids_SeenBNU(Implementation(MVotacao_i)) == (?: ?);
  List_Of_Ids(Machine(MVotacao_Ctx)) == (ESTADO,CPFs,ZONAS,preparacao,votacao,apuracao,cpf1,cpf2,cpf3,indefinido,zona1,zona2 | ? | ? | ? | ? | ? | ? | ? | MVotacao_Ctx);
  List_Of_HiddenCst_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MVotacao_Ctx)) == (?);
  List_Of_VisibleVar_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MVotacao_Ctx)) == (?: ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Implementation(MVotacao_i)) == (Type(registro_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(BOOL,0,1)));Type(votos_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(INTEGER,0,MAXINT)));Type(local_i) == Mvv(SetOf(etype(CPFs,0,3)*SetOf(etype(ZONAS,0,1)*btype(BOOL,0,1))));Type(candidatos_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(BOOL,0,1)));Type(eleitores_i) == Mvv(SetOf(etype(CPFs,0,3)*btype(BOOL,0,1)));Type(estado_i) == Mvv(etype(ESTADO,?,?)))
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(MVotacao_i),adicionar_eleitor, 1) == (Type(ii) == Lvl(btype(BOOL,?,?)));
  Variables_Loc(Implementation(MVotacao_i),remover_eleitor, 1) == (Type(ii) == Lvl(btype(BOOL,?,?));Type(oo) == Lvl(btype(BOOL,?,?)));
  Variables_Loc(Implementation(MVotacao_i),mudar_local, 1) == (Type(ii) == Lvl(btype(BOOL,?,?)));
  Variables_Loc(Implementation(MVotacao_i),adicionar_candidato, 1) == (Type(ii) == Lvl(btype(BOOL,?,?));Type(oo) == Lvl(btype(BOOL,?,?)));
  Variables_Loc(Implementation(MVotacao_i),remover_candidato, 1) == (Type(ii) == Lvl(btype(BOOL,?,?));Type(oo) == Lvl(btype(BOOL,?,?)));
  Variables_Loc(Implementation(MVotacao_i),iniciar_votacao, 1) == (Type(eleitor_1) == Lvl(btype(BOOL,?,?));Type(eleitor_2) == Lvl(btype(BOOL,?,?));Type(eleitor_3) == Lvl(btype(BOOL,?,?)));
  Variables_Loc(Implementation(MVotacao_i),votar, 1) == (Type(ee) == Lvl(btype(BOOL,?,?));Type(ii) == Lvl(btype(BOOL,?,?));Type(oo) == Lvl(btype(BOOL,?,?));Type(uu) == Lvl(btype(BOOL,?,?)));
  Variables_Loc(Implementation(MVotacao_i),apurar_vencedor, 1) == (Type(cand_1) == Lvl(btype(INTEGER,?,?));Type(cand_2) == Lvl(btype(INTEGER,?,?));Type(cand_3) == Lvl(btype(INTEGER,?,?)))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
&
THEORY ListLocalOperationsX IS
  List_Local_Operations(Implementation(MVotacao_i))==(?)
END
&
THEORY ListLocalInputX END
&
THEORY ListLocalOutputX END
&
THEORY ListLocalHeaderX END
&
THEORY ListLocalPreconditionX END
&
THEORY ListLocalSubstitutionX END
&
THEORY TypingPredicateX IS
  TypingPredicate(Implementation(MVotacao_i))==(estado_i: ESTADO & eleitores_i: POW(CPFs*BOOL) & candidatos_i: POW(CPFs*BOOL) & local_i: POW(CPFs*POW(ZONAS*BOOL)) & votos_i: POW(CPFs*INTEGER) & registro_i: POW(CPFs*BOOL))
END
&
THEORY ImportedVariablesListX END
&
THEORY ListLocalOpInvariantX END
)
