

SUBROUTINE calc_pente(Delta_u, pente)

USE Parametres

IMPLICIT NONE

REAL :: Delta_u, pente

pente = (Delta_u * f0) / g2

END SUBROUTINE calc_pente
