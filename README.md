Scripts to import, clean, organize, and visualize eyetracking data from TobiiProUnitySDK in the Virtual Storytelling experiment programs.<br><br>

![For Github](https://github.com/user-attachments/assets/caf3b40c-73df-479a-bacb-102b4abaf073) <br><br>

<b>importvisualizegaze.R:</b><br>
This script is to directly import, clean, and visualize data.<br><br>

<b>missinggazeretrieval.R:</b><br>
In the eye-tracking data that is exported by the experiment program, when participant looked at specific areas on the screen, this will be recorded as ”Lookat” variable. This Lookat variable can have seven values: Nothing, Nowhere, Background, Body, Face, Mouth, and Eyes (coded as 0, 1, 2, 3, 4, 5, 6 respectively during data analysis). When the participant’s eye gaze is null, the Lookat value will be Nothing (e.g., the participant’s eye gaze was not recognized or the participant did not look at the screen). When the eye gaze is not null, the Lookat value can be assigned with either the rest of the values: Nowhere when the eye gaze does not hit any objects (e.g., the participant’s eyes were closed or they did not look at the screen), Background when it hits the mesh collider assigned on the background, Body when it hits the box collider assigned to the avatar’s body area, Face when it hits the capsule collider assigned to the avatar’s face area, Mouth when it hits the box collider assigned to the avatar’s mouth area, and Eyes when it hits the box collider assigned to the avatar’s eyes area. However, these Lookat values were only assigned correctly in Trial 1. In Trial 2-6, there are only three Lookat values; Nothing, Nowhere or Background (see the plots in Figure 4). In order to fix and retrieve these missing Lookat information, I created this script to make use of the coordinate information from the available Lookat values in Trial 1. 
