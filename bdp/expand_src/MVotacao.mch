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
    , eleitores
    , candidatos
    , local
    , votos
    , registro

INVARIANT
    estado : ESTADO
    & eleitores : FIN ( CPFs )
    & candidatos : FIN ( CPFs )
    & local : eleitores +-> ZONAS
    & votos : candidatos +-> NAT
    & registro : eleitores +-> BOOL
    & ! aa . ( aa : eleitores => aa /= indefinido )
    & ! ee . ( ee : candidatos => ee /= indefinido )
    & ! ii . ( ii : candidatos => ii : eleitores )
    & ! oo . ( oo : ran ( registro ) & estado = preparacao => oo = FALSE )
    & ! uu . ( uu : ran ( votos ) & estado = preparacao => uu = 0 )

INITIALISATION
    estado := preparacao
    || eleitores := {}
    || candidatos := {}
    || local := {}
    || votos := {}
    || registro := {}

OPERATIONS

    adicionar_eleitor ( cpf , zona ) =
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


END
