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
