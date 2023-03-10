

  SUBROUTINE bruit2(kmin,kmax,par,grid)
    
    ! Une des multiples fonctons dont l'utilité est de créer du bruit sur un champs.
    
    USE Parametres
    IMPLICIT NONE

    
    INTEGER :: i,j,perio, kj, ki, ornEnabled, theEnd, par
    REAL    :: kij2, kmin, kmax, ax0, ay0, ornEps, ornPer, start, dice(6)
    REAL, dimension(0:Nx+1,0:Ny+1) :: grid
    REAL, dimension(Nx,Ny,6)       :: orn
    

    perio = 1

    !      Ornstein-Uhlenbeck

    !orn = 0
    !ornPer = tpi/f0
    !ornEps = 1.*dt/ornPer       

    !CALL cpu_time(start)
    !CALL init_random_seed
    
    theEnd = 0


    grid = 0

    DO kj = 0,Ny

       DO ki = 0,Nx

          IF (perio == 1) kij2 = ki**2+kj**2

          IF (perio == 2) kij2 = ki**2+kj**2/4

          IF (kij2 < kmax**2) THEN

             IF (kij2 >= kmin**2) THEN

                !IF (ornEnabled == 1) THEN

                   !ax0 = orn(ki,kj,1)
                   !ay0 = orn(ki,kj,2)

                !ELSE

                   CALL random_number(dice)
                   ax0 = tpi*dice(1)
                   ay0 = tpi*dice(2)

                !ENDIF

                IF (ki == 0) ax0 = asin(0.5)

                IF (kj == 0) ay0 = asin(0.5)

                IF (perio == 2) THEN

                   IF (par == -1) ay0 = 0
                   IF (par == +1) ay0 = pi/2

                ENDIF

                DO j = 0,Ny+1
                   DO i = 0,Nx+1

                      IF (perio == 1) grid(i,j) = grid(i,j)               &
         +sin((i-1)*ki*tpi/Nx+ax0)*sin((j-1)*kj*tpi/Ny+ay0)/sqrt(kij2)

                      IF (perio == 2) grid(i,j) = grid(i,j)               &
         +sin((i-1)*ki*tpi/Nx+ax0)*sin((j-1)*kj*pi/Ny +ay0)/sqrt(kij2)

                   ENDDO
                ENDDO
             ENDIF
          ENDIF
       ENDDO
    ENDDO


    grid = grid/maxval(grid)



  END SUBROUTINE bruit2
