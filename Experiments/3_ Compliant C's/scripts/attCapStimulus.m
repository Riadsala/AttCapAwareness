function [I x y] = attCapStimulus(targPos, distPos)

% targPos = position of target = [1,n]
% distPos = relative position of distracter = [0,n] (set to 0 for no distracter)

N = 1024; % size of output image[I x y] = attCapStimulus(targPos, distPos)
R = 256; % radius for circle presentation
r = 32; % radius of actual circles
n = 6; % number of circles


I = zeros(N,N);

circle = DrawCircleTemplate(r);

% draw
for i = 1:n
   phi = 2*i*pi/n;   
   x(i) = round(R * cos(phi)+N/2);
   y(i) = round(R * sin(phi)+N/2);   
   I((x(i)-r):(x(i)+r),(y(i)-r):(y(i)+r)) = ((i==targPos)+1)*circle;
end

% now draw distracter
if distPos~=0
    phi =  2*targPos*pi/n + 2*(distPos-1)*pi/n + pi/n;
    x(i+1) = round(R * cos(phi)+N/2);
    y(i+1) = round(R * sin(phi)+N/2);
    I((x(i+1)-r):(x(i+1)+r),(y(i+1)-r):(y(i+1)+r)) = -circle;
    
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