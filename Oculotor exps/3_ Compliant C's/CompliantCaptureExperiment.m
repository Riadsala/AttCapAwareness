 function experiment1


addpath('scripts/');

subjNum = input('input subject number: ');
stream = RandStream('mt19937ar', 'Seed', subjNum);
RandStream.setDefaultStream(stream);

iLink.doILink = 1;

nTrialsPerCond = 3 ; % number of trials

%% parameters
params.previewTime = 1;
params.stimulusTime = .8; %was .8

params.gray = 0.3;
params.previewColour = .1; % change this [0,1] to change colour of preview circles
params.previewSaturation = .5;

params.targetColour = 0.1;
params.targetSaturation = 0.5;

params.distracterColour = 0.05;
params.onsetColour = params.distracterColour;
params.saturation = 0.5;
params.value = 0.8;

% make trial list
targLocations = repmat(1:6, [12 1]);
distLocations = repmat([0 0 0 0 0 0 1 2 3 4 5 6], [6 1])';
trialList = repmat([targLocations(:), distLocations(:)], [2 1]);
trialList(1:(end/2),3) = 0;
trialList((end/2+1):end,3) = 1;
trialList = repmat(trialList, [2,1]);
trialList(1:(end/2),4) = 0;
trialList((end/2+1):end,4) = 1;
trialList = repmat(trialList, [nTrialsPerCond,1]);
trialList = trialList(randperm(length(trialList)), :);

clear targLocations distLocations

Screen('Preference', 'SkipSyncTests', 1);
%% set up psychtoolbox
bkgrndGreyLevel = round(255*params.gray);
N = 512;
stimuliScrn = Screen('OpenWindow',1, bkgrndGreyLevel);%, [001 01 1600 900]
[params.width, params.height] = Screen('WindowSize', stimuliScrn);%screen returns the size of the window
HideCursor;
%% check resolution is correct - quit if not!
if params.height~=1080
    quit
end
if params.width~=1920
    quit
end

%% set up various screens and stuff
Screen('TextFont', stimuliScrn, 'Helvetica');
Screen('TextSize', stimuliScrn, 30);
Screen('TextColor', stimuliScrn, [255 255 255])

% blank screen
blank = bkgrndGreyLevel*ones(N);
t_blank = Screen('MakeTexture', stimuliScrn, blank);
Screen('DrawTexture', stimuliScrn, t_blank);
Screen('Flip', stimuliScrn)
clear blank

% fixation cross
fixCross = makeFixCross(N, bkgrndGreyLevel);
t_fixCross = Screen('MakeTexture', stimuliScrn, fixCross);
clear fixCross

% make output file
fout = fopen(strcat('results/obs', int2str(subjNum), '.txt'), 'w');
fprintf(fout, 'observer, trial, targLoc, distLoc, targDiscrim, thoughtNoAttCap, tc, dc, RT, TrialTypeRT\n');

% set up eyelink
if iLink.doILink == 1
    iLink.edfdatafilename = strcat('attcap', int2str(subjNum));
    iLink = InitEyeLink(iLink, stimuliScrn);
end

%% run experiment!
nBlocks = 6; %changed from 12 to 6 AM
n= 0;

for block = 1:nBlocks
    
    % display start of block message
    Screen('DrawTexture', stimuliScrn, t_blank);
    DrawFormattedText(stimuliScrn, strcat('Press any key when ready to start block ', int2str(block), '/', int2str(nBlocks)), 'center', 'center');
    Screen('Flip', stimuliScrn)
    WaitSecs(0.1);
    KbWait;
    
    
    if block>1
        EyelinkDoTrackerSetup(iLink.el);
    end
    
    for t = 1:(nTrialsPerCond*6*12/nBlocks)
        n = n+1;
        [preview, stimulus tc dc] = createStimuli(trialList, n, params);
        t_preview  = Screen('MakeTexture', stimuliScrn, 255*preview);
        t_stimulus = Screen('MakeTexture', stimuliScrn, 255*stimulus);
        
        DoATrial;
        
        Screen('Close', [t_preview, t_stimulus]);
    end
    
end

if iLink.doILink
    Eyelink('ReceiveFile',[iLink.edfdatafilename]);
    Eyelink('closefile');
    Eyelink('shutdown');
end
ShowCursor;
fclose(fout);
sca

    function DoATrial        
        
        if iLink.doILink == 1
            Screen('DrawTexture', stimuliScrn, t_fixCross);
            Screen('Flip', stimuliScrn)
            EyelinkDoDriftCorrection(iLink.el, [], [], 0, 0);
            Eyelink('startrecording');
            Eyelink('message', strcat('TRIAL_START', int2str(n)));
        else
            %display fixation cross
            Screen('DrawTexture', stimuliScrn, t_fixCross);
            Screen('Flip', stimuliScrn)
            WaitSecs(0.2);
        end
        % display preview
        Screen('DrawTexture', stimuliScrn, t_preview);
        Screen('Flip', stimuliScrn)
        WaitSecs(params.previewTime);
        
        % display stimulus
        Screen('DrawTexture', stimuliScrn, t_stimulus);
        if iLink.doILink == 1
            Eyelink('message','STIM_ON');
        end
        Screen('Flip', stimuliScrn);
        currTime = getsecs(); 
        timeSecs = KbWait
        [keyIsDown,secs,keyCode] = KbCheck;
        responseTime = secs();
        response = getObserverInput('f', 'b');
        targDiscrim = response==tc;
        RT = responseTime - currTime;
        if iLink.doILink
            Eyelink('message', strcat('STIM_OFF', int2str(n)));
            
        end
        
        % display blank
        Screen('DrawTexture', stimuliScrn, t_blank);
        Screen('Flip', stimuliScrn);
        WaitSecs(0.2);
        if iLink.doILink
            Eyelink('message', strcat('TRIAL_END', int2str(n)));
            
        end
        %% display response text
        
%             response = getObserverInput('f', 'b');
%             targDiscrim = response==tc;
%         else
%             targDiscrim=-1;
%         end

        Screen('DrawTexture', stimuliScrn, t_blank);
        DrawFormattedText(stimuliScrn, 'Was this a good eye movement: yes (y) or no (n)?', 'center', 'center');
        Screen('Flip', stimuliScrn);
        TrialQuestion = getsecs();
        [keyIsDown,secs,keyCode] = KbCheck;
        TrialResponse = secs();
        WaitSecs(0.05);
        thoughtNoAttCap = getObserverInput('y', 'n');
        TrialTypeRT = TrialQuestion - TrialResponse;
        
        Screen('DrawTexture', stimuliScrn, t_blank);
        Screen('Flip', stimuliScrn);
        WaitSecs(0.1);

        Screen('DrawTexture', stimuliScrn, t_blank);
        Screen('Flip', stimuliScrn);
        Eyelink('Stoprecording');
        fprintf(fout, '%d, %d, %d, %d, %d, %d, %d, %d, %f, %f\n', subjNum, n, trialList(n,1), trialList(n,2), targDiscrim, thoughtNoAttCap, tc, dc, RT, TrialTypeRT);
        
    end

end

function [preview, stimulus tc dc] = createStimuli(trialList, n, params)

% create circle map for trial
N=1024;
[circles xt yt xtarg ytarg xdist ydist] = attCapStimulus(trialList(n,1),trialList(n,2));

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
[im_hsv tc] = DrawTarget(im_hsv, xtarg, ytarg, trialList(n,3));

stimulus = hsv2rgb(im_hsv);
stimulus(stimulus==0) = params.gray;

%% draw distrater symbol
if trialList(n,2)~=0
[im_hsv dc] = DrawDisracter(im_hsv, xdist, ydist,tc, trialList(n,4);

stimulus = hsv2rgb(im_hsv);
stimulus(stimulus==0) = params.gray;
else
    dc = 0;
end
end

%Warren if you want to make the C bigger change the c values below,
%currently it is c=10

function [im tc] = DrawTarget(im, xt, yt, direction)
c = 12;
d = 4;
if direction == 0
    tc = 1;
    im((xt-c):(xt-c+d), (yt-c):(yt+c),3) = 0;
    im( (xt-c):(xt+c),(yt-c):(yt-c+d),3) = 0;
    im((xt+c-d):(xt+c),(yt-c):(yt+c), 3) = 0;
else
    tc = -1;
    im((xt-c):(xt-c+d), (yt-c):(yt+c),3) = 0;
    im( (xt-c):(xt+c),(yt+c-d):(yt+c),3) = 0;
    im((xt+c-d):(xt+c),(yt-c):(yt+c), 3) = 0;
end
end


function [im dc] = DrawDisracter(im, xt, yt, tc, direction)
c = 12;
d = 4;
if tc == 1
if direction == 0
    dc = 1;
    im((xt-c):(xt-c+d), (yt-c):(yt+c),3) = 0;
    im( (xt-c):(xt+c),(yt-c):(yt-c+d),3) = 0;
    im((xt+c-d):(xt+c),(yt-c):(yt+c), 3) = 0;
else
    dc = -1;
    im((xt-c):(xt-c+d), (yt-c):(yt+c),3) = 0;
    im( (xt-c):(xt+c),(yt+c-d):(yt+c),3) = 0;
    im((xt+c-d):(xt+c),(yt-c):(yt+c), 3) = 0;
end
else
if rand < 0.5
    dc = 1;
    im((xt-c):(xt-c+d), (yt-c):(yt+c),3) = 0;
    im( (xt-c):(xt+c),(yt-c):(yt-c+d),3) = 0;
    im((xt+c-d):(xt+c),(yt-c):(yt+c), 3) = 0;
else
    dc = -1;
    im((xt-c):(xt-c+d), (yt-c):(yt+c),3) = 0;
    im( (xt-c):(xt+c),(yt+c-d):(yt+c),3) = 0;
    im((xt+c-d):(xt+c),(yt-c):(yt+c), 3) = 0;
end   
end
end


