
function fixCross = makeFixCross(N, grybk)
fixCross = grybk * ones(N);
fixCross(round(N/2), (round(N/2)-32):(round(N/2)+32)) = 255;
fixCross((round(N/2)-32):(round(N/2)+32), round(N/2)) = 255;
end