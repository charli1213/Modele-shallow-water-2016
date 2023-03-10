% Graphique Shallow-water 1D

clear all;
close all;

filename1 = 'Parametres.txt';

filename2 = 'H_t1.txt';
filename3 = 'H_t2.txt';
filename4 = 'H_t3.txt';
filename5 = 'H_t4.txt';

filename10 = 'H2_t1.txt';
filename11 = 'H2_t2.txt';
filename12 = 'H2_t3.txt';
filename13 = 'H2_t4.txt';

filename6 ='vort_t1.txt';
filename7 ='vort_t2.txt';
filename8 ='vort_t3.txt';
filename9 ='vort_t4.txt';


filename14 = 'u1_t1.txt';
filename15 = 'u1_t2.txt';
filename16 = 'u1_t3.txt';
filename18 = 'u1_t4.txt';

filename20 = 'v1_t1.txt';
filename21 = 'v1_t2.txt';
filename22 = 'v1_t3.txt';
filename23 = 'v1_t4.txt';



filename17 = 'Bottom.txt';

filename19 = 'energie.txt';
filename24 = 'var_h.txt';
filename25 = 'vort2_t4.txt';
filename26 = 'vort2_t2.txt';


delimiterIn = ' ';

Parametres = importdata(filename1,delimiterIn);

A = importdata(filename2,delimiterIn);
A2 = importdata(filename3,delimiterIn);
A3 = importdata(filename4,delimiterIn);
A4 = importdata(filename5,delimiterIn);

B = importdata(filename10,delimiterIn);
B2 = importdata(filename11,delimiterIn);
B3 = importdata(filename12,delimiterIn);
B4 = importdata(filename13,delimiterIn);

Vort1 = importdata(filename6,delimiterIn);
Vort2 = importdata(filename7,delimiterIn);
Vort3 = importdata(filename8,delimiterIn);
Vort4 = importdata(filename9,delimiterIn);

vort2_4 = importdata(filename25,delimiterIn);
vort2_2 = importdata(filename26,delimiterIn);

u1 = importdata(filename14,delimiterIn);
u2 = importdata(filename15,delimiterIn);
u3 = importdata(filename16,delimiterIn);
u4 = importdata(filename18,delimiterIn);

v1 = importdata(filename20,delimiterIn);
v2 = importdata(filename21,delimiterIn);
v3 = importdata(filename22,delimiterIn);
v4 = importdata(filename23,delimiterIn);

mvt = importdata('qnt_mvt.txt',delimiterIn);


b = importdata(filename17,delimiterIn);
energie = importdata(filename19,delimiterIn);
variation = importdata(filename24,delimiterIn);

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
Re   = Parametres(14);
Ri   = Parametres(15);
c    = Parametres(16);


Surface = H1*ones(Ny,Nx) + H2*ones(Ny,Nx) + A4;
Interface = H2*ones(Ny,Nx) + B4;



mvt1 = mvt(1,:);
mvt2 = mvt(2,:);




figure
subplot(2,2,1)
%contourf(Vort1)
surf(A)
shading flat
%zlim([-300,300])
xlabel('Ouest')
ylabel('Nord')
title('Temps initial (Première couche)')

subplot(2,2,2)
%contourf(Vort2)
surf(A2)
shading flat
%zlim([-300,300])
title('Nt/3')

subplot(2,2,3)
%contourf(Vort3)
surf(A3)
shading flat
%zlim([-300,300])
title('2Nt/3')

subplot(2,2,4)
%contourf(Vort4)
surf(A4)
shading flat
%zlim([-300,300])
title('Temps final')





figure
subplot(2,2,1)
%contourf(Vort1)
surf(B)
shading flat
%zlim([-300,300])
xlabel('Ouest')
ylabel('Nord')
title('Temps initial (Deuxième couche)')

subplot(2,2,2)
%contourf(Vort2)
surf(B2)
shading flat
%zlim([-300,300])
title('Nt/3')

subplot(2,2,3)
%contourf(Vort3)
surf(B3)
shading flat
%zlim([-300,300])
title('2Nt/3')

subplot(2,2,4)
%contourf(Vort4)
surf(B4)
shading flat
%zlim([-300,300])
title('Temps final')






figure 

caxis(2E-7*[0.7 1])
colorbar
shading flat
subplot(2,2,1)
imagesc(Vort1)
%surf(B)
%zlim([-300,300])
title('Temps initial : vorticité')

subplot(2,2,2)
imagesc(Vort2)
colorbar
%surf(B2)
%zlim([-300,300])
title('Nt/3 : vorticité')

subplot(2,2,3)
imagesc(Vort3)
colorbar
%surf(B3)
%zlim([-300,300])
title('2Nt/3 : vorticité')

subplot(2,2,4)
imagesc(Vort4)
colorbar
%surf(B4)
%zlim([-300,300])
title('Temps final : vorticité')

%figure
%surf(Surface)
%hold on
%surf(Interface)
%hold on
%surf(b)

%figure 
%plot(mvt1)

figure
imagesc(u4)
colorbar
shading flat
title('Vitesse finale : u')


figure 
imagesc(v4)
colorbar
shading flat
title('Vitesse finale : v')

figure 
imagesc(vort2_2)
colorbar
shading flat
title('Vorticité finale : en bas')

%figure 
%plot(variation)