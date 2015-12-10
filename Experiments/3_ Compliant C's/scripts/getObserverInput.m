
function [resp, responseKeyHit] = getObserverInput(c1, c2);
responseKeyHit=0;
while responseKeyHit==0
    % check if a key has been pressed
    [keyIsDown, ~, keyCode] = KbCheck;
    % wait for keypress
    while ~keyIsDown
        [keyIsDown, ~, keyCode] = KbCheck;
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
        responseKeyHit = 0;
    end
end
end
