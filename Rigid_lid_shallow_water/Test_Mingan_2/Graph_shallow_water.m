% Deuxieme version graphiques

clear all;
close all;

Parametres = importdata('Parametres.txt',' ');


Nx   = Parametres(1);
Lx   = Parametres(2);
dx   = Parametres(3);
Ny   = Parametres(4);
Ly   = Parametres(5);
dy   = Parametres(6);
Nt   = Parametres(7);
dt   = Parametres(8);
H1   = Parametres(9);
H2   = Parametres(10);
f0   = Parametres(11);
Beta = Parametres(12);
mu   = Parametres(13);
count = Parametres(14)-1 ;

for i = 1:count
    
s1 = sprintf('%05d',i);
s2 = num2str(s1);
    
%n = num2str(i);

%s3 = strcat('0000',n,'.txt');

s = strcat(s2,'.txt');

filename1 = strcat('ubc_',s);
filename2 = strcat('vbc_',s);
filename3 = strcat('eta_',s);
filename4 = strcat('psi_',s);

ubc(:,:,i) = importdata(filename1,' ');
vbc(:,:,i) = importdata(filename2,' ');
eta(:,:,i) = importdata(filename3,' ');
psi(:,:,i) = importdata(filename4,' ');

end



% figure
% subplot(2,2,1)
% surf(eta(:,:,1))
% 
% subplot(2,2,2)
% surf(eta(:,:,2))
% 
% subplot(2,2,3)
% surf(eta(:,:,3))
% 
% subplot(2,2,4)
% surf(eta(:,:,4))


figure
subplot(2,2,1)
imagesc(eta(:,:,1)')
colorbar
title('eta')
set(gca, 'YDir', 'normal')

subplot(2,2,2)
imagesc(psi(:,:,1)')
colorbar
title('psi')
set(gca, 'YDir', 'normal')

subplot(2,2,3)
imagesc(ubc(:,:,1)')
colorbar
title('ubc')
set(gca, 'YDir', 'normal')

subplot(2,2,4)
imagesc(vbc(:,:,1)')
colorbar
title('vbc')
set(gca, 'YDir', 'normal')

figure
subplot(2,2,1)
imagesc(eta(:,:,2)')
colorbar
title('eta')
set(gca, 'YDir', 'normal')

subplot(2,2,2)
imagesc(psi(:,:,2)')
colorbar
title('psi')
set(gca, 'YDir', 'normal')

subplot(2,2,3)
imagesc(ubc(:,:,2)')
colorbar
title('ubc')
set(gca, 'YDir', 'normal')

subplot(2,2,4)
imagesc(vbc(:,:,2)')
colorbar
title('vbc')
set(gca, 'YDir', 'normal')

figure
subplot(2,2,1)
imagesc(eta(:,:,count)')
colorbar
title('eta')
set(gca, 'YDir', 'normal')

subplot(2,2,2)
imagesc(psi(:,:,count)')
colorbar
title('psi')
set(gca, 'YDir', 'normal')

subplot(2,2,3)
imagesc(ubc(:,:,count)')
colorbar
title('ubc')
set(gca, 'YDir', 'normal')

subplot(2,2,4)
imagesc(vbc(:,:,count)')
colorbar
title('vbc')
set(gca, 'YDir', 'normal')



