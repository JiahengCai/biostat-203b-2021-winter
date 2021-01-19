#!/bin/bash
FILE=$(tr -s '[[:punct:][:space:]]' '\n' < pride_and_prejudice.txt)
elizacount=0
janecount=0
lydiacount=0
darcycount=0
for w in $FILE
do
	if [[ $w =~ Jane ]]
	then
		janecount=$((janecount+1))
	elif [[ $w =~ Elizabeth ]]
	then
		elizacount=$((elizacount+1))
	elif [[ $w =~ Lydia ]]
	then
		lydiacount=$((lydiacount+1))
	elif [[ $w =~ Darcy ]]
	then
		darcycount=$((darcycount+1))
	fi
done
echo Elizabeth count is $elizacount
echo Jane count is $janecount
echo Lydia count is $lydiacount
echo Darcy count is $darcycount
