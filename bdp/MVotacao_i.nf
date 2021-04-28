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
  List_VisibleVariables(Implementation(MVotacao_i))==(registro,votos,local,candidatos,eleitores,estado);
  Internal_List_VisibleVariables(Implementation(MVotacao_i))==(registro,votos,local,candidatos,eleitores,estado)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(MVotacao_i))==(btrue);
  Expanded_List_Invariant(Implementation(MVotacao_i))==(btrue);
  Abstract_List_Invariant(Implementation(MVotacao_i))==(estado: ESTADO & eleitores: FIN(CPFs) & candidatos: FIN(CPFs) & local: eleitores +-> ZONAS & votos: candidatos +-> NAT & registro: eleitores +-> BOOL & !aa.(aa: eleitores => aa/=indefinido) & !ee.(ee: candidatos => ee/=indefinido) & !ii.(ii: candidatos => ii: eleitores) & !oo.(oo: ran(registro) & estado = preparacao => oo = FALSE) & !uu.(uu: ran(votos) & estado = preparacao => uu = 0));
  Context_List_Invariant(Implementation(MVotacao_i))==(btrue);
  List_Invariant(Implementation(MVotacao_i))==(btrue)
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
  Expanded_List_Initialisation(Implementation(MVotacao_i))==(estado:=preparacao;eleitores:={};candidatos:={};local:={};votos:={};registro:={});
  Context_List_Initialisation(Implementation(MVotacao_i))==(skip);
  List_Initialisation(Implementation(MVotacao_i))==(estado:=preparacao;eleitores:={};candidatos:={};local:={};votos:={};registro:={})
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
  Expanded_List_Substitution(Implementation(MVotacao_i),apurar_vencedor)==(estado = apuracao | #vencedor.(vencedor: candidatos & vencedor: dom(votos) & !aa.(aa: candidatos & aa/=vencedor => votos(vencedor)>votos(aa)) & votos(vencedor)>SIGMA(ee).(ee: ran(votos) | ee)/2) ==> (vencedor: dom(votos) & aa: dom(votos) & SIGMA(ee).(ee: ran(votos) | ee)/2: INT & SIGMA(ee).(ee: ran(votos) | ee): INT & 2: INT & not(2 = 0) | resultado:={venc | #vencedor.(vencedor: candidatos & vencedor: dom(votos) & !aa.(aa: candidatos & aa/=vencedor => votos(vencedor)>votos(aa)) & votos(vencedor)>SIGMA(ee).(ee: ran(votos) | ee)/2 & venc = vencedor)}) [] not(#vencedor.(vencedor: candidatos & vencedor: dom(votos) & !aa.(aa: candidatos & aa/=vencedor => votos(vencedor)>votos(aa)) & votos(vencedor)>SIGMA(ee).(ee: ran(votos) | ee)/2)) ==> resultado:={indefinido});
  Expanded_List_Substitution(Implementation(MVotacao_i),concluir_votacao)==(estado = votacao | estado:=apuracao);
  Expanded_List_Substitution(Implementation(MVotacao_i),votar)==(estado = votacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & zona: ZONAS & cpf: dom(local) & cpf: dom(registro) & zona = local(cpf) & cpf_candidato: CPFs & cpf_candidato: candidatos\/{indefinido} & registro(cpf) = FALSE | cpf_candidato: candidatos & cpf_candidato: dom(votos) & votos(cpf_candidato)+1: NAT ==> (cpf_candidato: dom(votos) & votos(cpf_candidato)+1: INT & votos(cpf_candidato): INT & 1: INT | votos:=votos<+{cpf_candidato|->votos(cpf_candidato)+1}) [] not(cpf_candidato: candidatos & cpf_candidato: dom(votos) & votos(cpf_candidato)+1: NAT) ==> skip;(cpf: dom(registro) | registro:=registro<+{cpf|->TRUE}));
  Expanded_List_Substitution(Implementation(MVotacao_i),iniciar_votacao)==(estado = preparacao & candidatos/={} | estado:=votacao);
  Expanded_List_Substitution(Implementation(MVotacao_i),remover_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf: candidatos | votos:={cpf}<<|votos;candidatos:=candidatos-{cpf});
  Expanded_List_Substitution(Implementation(MVotacao_i),adicionar_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf/:candidatos | candidatos:=candidatos\/{cpf};votos:=votos\/{cpf|->0});
  Expanded_List_Substitution(Implementation(MVotacao_i),mudar_local)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf: dom(local) & zona: ZONAS & zona/=local(cpf) & cpf/=indefinido | local:=local<+{cpf|->zona});
  Expanded_List_Substitution(Implementation(MVotacao_i),remover_eleitor)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido | cpf: candidatos ==> (votos:={cpf}<<|votos;candidatos:=candidatos-{cpf}) [] not(cpf: candidatos) ==> skip;local:={cpf}<<|local;registro:={cpf}<<|registro;eleitores:=eleitores-{cpf});
  Expanded_List_Substitution(Implementation(MVotacao_i),adicionar_eleitor)==(estado = preparacao & cpf: CPFs & cpf/:eleitores & zona: ZONAS & cpf/=indefinido | eleitores:=eleitores\/{cpf};local:=local\/{cpf|->zona};registro:=registro\/{cpf|->FALSE});
  List_Substitution(Implementation(MVotacao_i),adicionar_eleitor)==(eleitores:=eleitores\/{cpf};local:=local\/{cpf|->zona};registro:=registro\/{cpf|->FALSE});
  List_Substitution(Implementation(MVotacao_i),remover_eleitor)==(IF cpf: candidatos THEN votos:={cpf}<<|votos;candidatos:=candidatos-{cpf} END;local:={cpf}<<|local;registro:={cpf}<<|registro;eleitores:=eleitores-{cpf});
  List_Substitution(Implementation(MVotacao_i),mudar_local)==(local:=local<+{cpf|->zona});
  List_Substitution(Implementation(MVotacao_i),adicionar_candidato)==(candidatos:=candidatos\/{cpf};votos:=votos\/{cpf|->0});
  List_Substitution(Implementation(MVotacao_i),remover_candidato)==(votos:={cpf}<<|votos;candidatos:=candidatos-{cpf});
  List_Substitution(Implementation(MVotacao_i),iniciar_votacao)==(estado:=votacao);
  List_Substitution(Implementation(MVotacao_i),votar)==(IF cpf_candidato: candidatos & cpf_candidato: dom(votos) & votos(cpf_candidato)+1: NAT THEN votos:=votos<+{cpf_candidato|->votos(cpf_candidato)+1} END;registro(cpf):=TRUE);
  List_Substitution(Implementation(MVotacao_i),concluir_votacao)==(estado:=apuracao);
  List_Substitution(Implementation(MVotacao_i),apurar_vencedor)==(IF #vencedor.(vencedor: candidatos & vencedor: dom(votos) & !aa.(aa: candidatos & aa/=vencedor => votos(vencedor)>votos(aa)) & votos(vencedor)>SIGMA(ee).(ee: ran(votos) | ee)/2) THEN resultado:={venc | #vencedor.(vencedor: candidatos & vencedor: dom(votos) & !aa.(aa: candidatos & aa/=vencedor => votos(vencedor)>votos(aa)) & votos(vencedor)>SIGMA(ee).(ee: ran(votos) | ee)/2 & venc = vencedor)} ELSE resultado:={indefinido} END)
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
  VisibleVariables(Implementation(MVotacao_i))==(Type(estado) == Mvv(etype(ESTADO,?,?));Type(eleitores) == Mvv(SetOf(etype(CPFs,?,?)));Type(candidatos) == Mvv(SetOf(etype(CPFs,?,?)));Type(local) == Mvv(SetOf(etype(CPFs,?,?)*etype(ZONAS,?,?)));Type(votos) == Mvv(SetOf(etype(CPFs,?,?)*btype(INTEGER,?,?)));Type(registro) == Mvv(SetOf(etype(CPFs,?,?)*btype(BOOL,?,?))));
  Operations(Implementation(MVotacao_i))==(Type(apurar_vencedor) == Cst(SetOf(etype(CPFs,?,?)),No_type);Type(concluir_votacao) == Cst(No_type,No_type);Type(votar) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?)*etype(CPFs,?,?));Type(iniciar_votacao) == Cst(No_type,No_type);Type(remover_candidato) == Cst(No_type,etype(CPFs,?,?));Type(adicionar_candidato) == Cst(No_type,etype(CPFs,?,?));Type(mudar_local) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?));Type(remover_eleitor) == Cst(No_type,etype(CPFs,?,?));Type(adicionar_eleitor) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?)))
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
  List_Of_VisibleVar_Ids(Implementation(MVotacao_i)) == (registro,votos,local,candidatos,eleitores,estado | ?);
  List_Of_Ids_SeenBNU(Implementation(MVotacao_i)) == (?: ?);
  List_Of_Ids(Machine(MVotacao_Ctx)) == (ESTADO,CPFs,ZONAS,preparacao,votacao,apuracao,cpf1,cpf2,cpf3,indefinido,zona1,zona2 | ? | ? | ? | ? | ? | ? | ? | MVotacao_Ctx);
  List_Of_HiddenCst_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MVotacao_Ctx)) == (?);
  List_Of_VisibleVar_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MVotacao_Ctx)) == (?: ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Implementation(MVotacao_i)) == (Type(registro) == Mvv(SetOf(etype(CPFs,?,?)*btype(BOOL,?,?)));Type(votos) == Mvv(SetOf(etype(CPFs,?,?)*btype(INTEGER,?,?)));Type(local) == Mvv(SetOf(etype(CPFs,?,?)*etype(ZONAS,?,?)));Type(candidatos) == Mvv(SetOf(etype(CPFs,?,?)));Type(eleitores) == Mvv(SetOf(etype(CPFs,?,?)));Type(estado) == Mvv(etype(ESTADO,?,?)))
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
  TypingPredicate(Implementation(MVotacao_i))==(estado: ESTADO & eleitores: POW(CPFs) & candidatos: POW(CPFs) & local: POW(CPFs*ZONAS) & votos: POW(CPFs*INTEGER) & registro: POW(CPFs*BOOL))
END
&
THEORY ImportedVariablesListX END
&
THEORY ListLocalOpInvariantX END
)
