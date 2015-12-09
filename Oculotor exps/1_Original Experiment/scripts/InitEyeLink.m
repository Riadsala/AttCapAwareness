function [eyeLink] = InitEyeLink( eyeLink , w)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

     %initialize eyelink
    if EyelinkInit()~= 1; %
    	return;
    end;
    % 
    eyeLink.centralTol = 30^2; % how close does the observer have to be

        %lets get it working without eylink for now
        % do eyelink stuff
    eyeLink.el=EyelinkInitDefaults(w);
    iLink.window = w;
    
     eyeLink.el.backgroundcolour = 77;%(BlackIndex(w)+WhiteIndex(w))/2;
     eyeLink.el.foregroundcolour = BlackIndex(w);
     eyeLink.el.targetbeep =0;
     %eyeLink.el.drift_correction_success_beep=[800 0.0 0.25];%volume to 0.0
    eyeLink.el.eye_used = eyeLink.el.RIGHT_EYE; % use right eye  +1 for matlab array offset!!!!

    if ~isempty(eyeLink.el.window) & ~isempty(eyeLink.el.callback) %#ok<AND2>
         PsychEyelinkDispatchCallback(eyeLink.el);     %then pass it to the callback
     end
    
   
    %   SET UP TRACKER CONFIGURATION
    Eyelink('command', 'calibration_type = HV9'); 
    %	set parser (conservative saccade thresholds)
    Eyelink('command', 'saccade_velocity_threshold = 35');
    Eyelink('command', 'saccade_acceleration_threshold = 9500');
    %	set EDF file contents
    Eyelink('command', 'file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON');
    %	set link data (used for gaze cursor)
    Eyelink('command', 'link_event_filter = LEFT,RIGHT,FIXATION,BUTTON, SACCADE');
    Eyelink('command', 'link_sample_data  = LEFT,RIGHT,GAZE,AREA,SACCADE,BLINK');



    % open file to record data to
    Eyelink('openfile', eyeLink.edfdatafilename);%string defined earlier
 
    % STEP 4
    % Calibrate the eye tracker
    EyelinkDoTrackerSetup(eyeLink.el);

    % do a final check of calibration using driftcorrection
    %EyelinkDoDriftCorrection(eyeLink.el);

    WaitSecs(0.1);
    Eyelink('StartRecording');

    %eyeLink.eye_used = Eyelink('EyeAvailable')+1; % get eye that's tracked...+1 for matlab array offset!!!!
    %if eyeLink.eye_used == eyeLink.el.BINOCULAR; % if both eyes are tracked
    %end



    
end

