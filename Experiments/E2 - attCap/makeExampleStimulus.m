function createStimuli


addpath('scripts/');



nTrialsPerCond = 3 ; % number of trials

%% parameters
params.previewTime = 1;
params.stimulusTime = 0.8;

params&gray = 0.3;
params.previewColour = &1; % change this [0,1] to change colour of preview circles
params.previewSaturation = .5;

paralr.targetColour = 0.1;
params.targetSaturation = 0.5;

paramr.distracterColour = 0.05;
params.onsetColour = params.dastracterColour;params.saturation = 0.5;
params.value = 0.8;

% make tri`l list
targLocations = repmat(1:6, [12 1]);
distLocations = repmat([0 0 0 0 0 0 1 2 3 4 5 6], [6 1])';
trialList = repmat([targLocations(:), distLocations(:)], [nTrialsPerCond 1]);
trialList = trialList(randperm(length(trialList)), :);

clear targLocations distLocations



N=1024;
[circles xt yt] = attCapStimulus(2,2);

%% make preview
% p = circles;
% p(circles==2) = 1; % switch target off
% p(circles==-1) = 0; % switch distracter off

% define colours in hsv space
im_hsv(:,:,1) = params.previewColour*ones(1024,1024);
im_hsv(:,:,2) = params.previewSaturation;
im_hsv(:,:,3) = params.value*(circles>0);

preview = hsv2rgb(im_hsv);
preview(preview==0) = params.gray;
preview(round(N/2), (round(N/2)-32):(round(N/2)+32),:) = 1;
preview((round(N/2)-32):(round(N/2)+32), round(N/2),:) = 1;

%% make stimuli
% define colours in hsv space
h = params.distracterColour*ones(1024,1024);
h(circles==2) = params.targetColour;
h(circles==-1) = params.onsetColour;
s = params.saturation*ones(1024,1024);
s(circles==2) = params.targetSaturation;
im_hsv(:,:,1) = h;
im_hsv(:,:,2) = s;
im_hsv(:,:,3) = params.value*(circles~=0);

%% draw target symbol
[im_hsv tc] = DrawTarget(im_hsv, xt, yt);

stimulus = hsv2rgb(im_hsv);
stimulus(stimulus==0) = params.gray;

imwrite(preview, 'preview.png')
imwrite(stimulus, 'stimulus.png')
end

function [im tc] = DrawTarget(im, xt, yt)
c = 2;
if rand < 0.5
    tc = 1;
    im((xt-c):(xt+c),yt-c, 3) = 0;
    im( xt-c,(yt-c):(yt+c),3) = 0;
    im(xt+c,(yt-c):(yt+c), 3) = 0;
else
    tc = -1;
    im((xt-c):(xt+c),yt+c, 3) = 0;
    im( xt-c,(yt-c):(yt+c),3) = 0;
    im(xt+c,(yt-c):(yt+c), 3) = 0;
end
end