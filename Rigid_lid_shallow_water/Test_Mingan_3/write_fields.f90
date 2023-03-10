


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



count = count + 1
