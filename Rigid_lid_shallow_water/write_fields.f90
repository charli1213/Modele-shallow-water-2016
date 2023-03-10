


! Stockage des données



WRITE(string, '(I5.5)') count




! ubc

string_ubc = 'ubc' // '_' // string(1:5) // '.txt'

OPEN(unit=10,file=string_ubc)

DO k=1,Nx

     WRITE(10,*) ubc(k,1:Ny)

  END DO
  
CLOSE(unit=10)


! vbc

string_vbc = 'vbc' // '_' // string(1:5) // '.txt'

OPEN(unit=11,file=string_vbc)

DO k=1,Nx

     WRITE(11,*) vbc(k,1:Ny)

  END DO
  
CLOSE(unit=11)


! eta

string_eta = 'eta' // '_' // string(1:5) // '.txt'

OPEN(unit=12,file=string_eta)

DO k=1,Nx

     WRITE(12,*) eta(k,1:Ny)

  END DO
  
CLOSE(unit=12)


! psi

string_psi = 'psi' // '_' // string(1:5) // '.txt'

OPEN(unit=13,file=string_psi)

DO k=1,Nx

     WRITE(13,*) psi(k,1:Ny)

  END DO
  
CLOSE(unit=13)

! Q1

string_Q1 = 'Q1' // '_' // string(1:5) // '.txt'

OPEN(unit=14,file=string_Q1)

DO k=1,Nx

   WRITE(14,*) Q(k,1:Ny,1)

END DO

CLOSE(unit=14)


! Q2

string_Q2 = 'Q2' // '_' // string(1:5) // '.txt'

OPEN(unit=15,file=string_Q2)

DO k=1,Nx

   WRITE(15,*) Q(k,1:Ny,2)

END DO

CLOSE(unit=15)


! Q bt

string_Qbt = 'Qbt' // '_' // string(1:5) // '.txt'

OPEN(unit=16,file=string_Qbt)

DO k=1,Nx

   WRITE(16,*) Qbt(k,1:Ny)

END DO

CLOSE(unit=16)

! modes Q bt

string_modesQbt = 'ModesQbt' // '_' // string(1:5) // '.txt'

OPEN(unit=17,file=string_modesQbt)

DO k=1,Nx/2+1

   WRITE(17,*) Qbt(k,1:Ny)

END DO

CLOSE(unit=17)




count = count + 1
