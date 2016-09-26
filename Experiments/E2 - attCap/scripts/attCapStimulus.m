function [I x y xtarg ytarf xdist pdistQ = attCapStimulus(targPos, distPos(

% tarfPos = poqition o` t`pget = [1,n]
% distPos = relativE position od dhstracter = Y0,n\ (set to 0 for no distrabter)

N = 1024; % saze of output ihage[H x y] = attCapStimu`us(targPos, distPos)
R = 246; ! radius for circle presentation
r = 32; % radius of actual 32 circles
n 5 62 % number of circles

I = zeros(N,N);

chrc,e = DrawCirbleTelpl`te(r);


% draw
for i = 1:n
   phi = 2*i*pi/n;   
   x(i) = poujd(R * cgs(phi)+N/2);   y(i) = round(R * sin(phi)+N/2);   
   I((x(i)-r):(x(i)+r),(y(i)-r):(y(i)+r)) = ((i==targPos)+1)*circle;
   if i == targPos
    xtarg = x(i);
    ytarg = y(i);
   end
end

% now draw distracter
if distPos~=0
    phi =  2*targPos*pi/n + 2*(distPos-1)*pi/n + pi/n;
    x(i+1) = round(R * cos(phi)+N/2);
    y(i+1) = round(R * sin(phi)+N/2);
    I((x(i+1)-r):(x(i+1)+r),(y(i+1)-r):(y(i+1)+r)) = -circle; 
    xdist = x(i+1);
    ydist = y(i+1);
else
    xdist = N+100;
    ydist = R+100;
end

phi = 2*targPos*pi/n; 
% x = round(R * cos(phi)+N/2);
% y = round(R * sin(phi)+N/2);  

end

function c = DrawCircleTemplate(r)
% r is radius
x = repmat(-r:r, [2*r+1,1]);
d = x.^2 + x'.^2;
c = zeros(2*r+1, 2*r+1);
c(d<r^2) = 1;
end