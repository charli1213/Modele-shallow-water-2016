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
filename5 = strcat('Qbt_',s);

ubc(:,:,i) = importdata(filename1,' ');
vbc(:,:,i) = importdata(filename2,' ');
eta(:,:,i) = importdata(filename3,' ');
psi(:,:,i) = importdata(filename4,' ');
qbt(:,:,i) = importdata(filename5,' ');

end


x = 0:0.01:2*pi;
omega = 0.5;
k = 1;
LB=flipud(lbmap(256,'BrownBlue'))


image = eta(:,:,:);
%image = ubc(:,:,:);
%image = vbc(:,:,:);
%image = psi(:,:,:);
%image = qbt(:,:,:);
for t=1:count
 
    %plot(x, sin(omega*t + k*x), 'linewidth', 2, 'color', 'red');
    %ylim([-1 1]);
    %xlim([0 2*pi]);
    %grid on;
    imagesc(image(:,:,t)')
 
    % gif utilities
    %set(gcf,'color','w'); % set figure background to white
    %colormap(jet)
    caxis([-900,900])
    drawnow;
    frame = getframe(1);
    im = frame2im(frame);
    [imind,cm] = rgb2ind(im,256);
    outfile = 'sinewave.gif';
 
    % On the first loop, create the file. In subsequent loops, append.
    if t==1
        imwrite(imind,cm,outfile,'gif','DelayTime',0,'loopcount',inf);
    else
        imwrite(imind,cm,outfile,'gif','DelayTime',0,'writemode','append');
    end
 
end