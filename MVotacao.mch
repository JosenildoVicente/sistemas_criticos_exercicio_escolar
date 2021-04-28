/* MVotacao
 * Author: Josenildo Vicente
 * Creation date: 26/04/2021
 */
MACHINE
    MVotacao
    
SEES
    MVotacao_Ctx
    
ABSTRACT_VARIABLES
    estado
    ,eleitores
    ,candidatos
    ,local
    ,votos
    ,registro
    
INVARIANT
    estado : ESTADO
    & eleitores : FIN(CPFs)
    & candidatos : FIN(CPFs)
    & local : eleitores +-> ZONAS
    & votos : candidatos +-> NAT
    & registro : eleitores +-> BOOL
    & ! aa . ( aa : eleitores => aa /= indefinido )
    & ! ee . ( ee : candidatos => ee /= indefinido )
    & ! ii . ( ii : candidatos => ii : eleitores )
    & ! oo . ( oo : ran(registro) & estado = preparacao => oo = FALSE )
    & ! uu . ( uu : ran(votos) & estado = preparacao => uu = 0 )

INITIALISATION
    estado := preparacao
    || eleitores := {}
    || candidatos := {}
    || local := {}
    || votos := {}
    || registro := {} 
    
OPERATIONS
    
    adicionar_eleitor ( cpf, zona ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf /: eleitores
        & zona : ZONAS
        & cpf /= indefinido
    THEN
       eleitores := eleitores \/ { cpf }
       || local := local \/ { cpf |-> zona }
       || registro := registro \/ { cpf |-> FALSE }
    END
    ;
    
    remover_eleitor ( cpf ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
    THEN
        IF cpf : candidatos THEN votos := { cpf } <<| votos  || candidatos := candidatos - { cpf } END
        || local := { cpf } <<| local
        || registro := { cpf } <<| registro
        || eleitores := eleitores - { cpf }
    END
    ;
    
    mudar_local ( cpf, zona ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf : dom(local)
        & zona : ZONAS
        & zona /= local(cpf)
        & cpf /= indefinido
    THEN
        local:= local <+ {cpf |-> zona}
    END
    ;
    
    adicionar_candidato ( cpf ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
        & cpf /: candidatos
    THEN
       candidatos := candidatos \/ { cpf }
       || votos := votos \/ { cpf |-> 0 }
    END
    ;
    
    remover_candidato ( cpf ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
        & cpf : candidatos
    THEN
       votos := { cpf } <<| votos
       || candidatos := candidatos - { cpf }
    END
    ;
    
    iniciar_votacao =
    PRE
        estado = preparacao
        & candidatos /= {}
    THEN
       estado := votacao
    END
    ;
    
    votar ( cpf, zona, cpf_candidato)=
    PRE
        estado = votacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
        & zona : ZONAS
        & cpf : dom(local)
        & cpf : dom(registro)
        & zona = local(cpf)
        & cpf_candidato : CPFs
        & (cpf_candidato : (candidatos \/ { indefinido} ))
//        & cpf_candidato : dom(votos)
        & registro(cpf) = FALSE
//        & {cpf_candidato |-> votos(cpf_candidato) + 1} : candidatos +-> NAT
    THEN
        IF cpf_candidato : candidatos & cpf_candidato : dom(votos) & (votos(cpf_candidato) + 1) : NAT
        THEN
            votos := votos <+ { cpf_candidato |-> ( votos(cpf_candidato) + 1 ) }
        END
        || registro(cpf) := TRUE
    END
    ;
    
    concluir_votacao =
    PRE
        estado = votacao
    THEN
       estado := apuracao
    END
    ;
    
    resultado <-- apurar_vencedor =
    PRE
        estado = apuracao
    THEN
        IF # vencedor . (vencedor : candidatos & vencedor : dom(votos) & ( ! aa . ( aa : candidatos & aa /= vencedor => votos(vencedor) > votos(aa))) & (votos(vencedor) > ((SIGMA ee . ( ee : ran(votos) | ee ))/2 )) )
        THEN
            resultado := {venc | # vencedor . (vencedor : candidatos & vencedor : dom(votos) & ( ! aa . ( aa : candidatos & aa /= vencedor => votos(vencedor) > votos(aa))) & (votos(vencedor) > ((SIGMA ee . ( ee : ran(votos) | ee ))/2 )) & venc = vencedor) }
        ELSE
            resultado := {indefinido}
        END

    END

END
