
function [resp, responseKeyHit, rt] = getObserverInput(c1, c2, timeLimit); %put in RT
responseKeyHit=0;
t0 = GetSecs();
t=t0;
while responseKeyHit==0
    % check if a key has been pressed
    [keyIsDown, ~, keyCode] = KbCheck;
    % wait for keypress
    while ~keyIsDown
        [keyIsDown, t, keyCode] = KbCheck;
        %      check we haven't ran out of time

        if t-t0 > timeLimit 
            keyIsDown = 1; 
        end
        WaitSecs(0.005);
    end
    % check what key was pressed
    if find(keyCode) == KbName(c1);
        responseKeyHit = 1;
        resp = 1;
    elseif find(keyCode) == KbName(c2);
        responseKeyHit = 1;
        resp = -1;
    elseif  find(keyCode) == KbName('q');
        responseKeyHit = 1;
        resp = 3;
    else
        resp = 3;
         responseKeyHit = 1;
    end
    
end
rt = t-t0;