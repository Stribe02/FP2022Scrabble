# FP2022Scrabble
Scrabble Project for FP2022

Deleting old nuget:

dotnet nuget remove source FP2022 


Adding new nuget:

dotnet nuget add source https://nuget.pkg.github.com/jesper-bengtson/index.json -n FP2022 -u jesper-bengtson -p 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


Pushing nuget:
REPLACE Xs WITH ACTUAL VERSION NUMBER!
While standing in ScrabbelBot

dotnet nuget push bin/Debug/Wordfeud.X.X.X.nupkg --source https://nuget.pkg.github.com/jesper-bengtson/index.json --api-key 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
