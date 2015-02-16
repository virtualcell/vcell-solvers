function [D_Inv, Directions, MidPoints] = GetBoundaryIndependentMetrics(NatNeighborPairs, PosX, PosY)

I = NatNeighborPairs(:,1);
J = NatNeighborPairs(:,2);
DirectionsX = PosX(I)-PosX(J);
DirectionsY = PosY(I)-PosY(J);
Lengths = sqrt(DirectionsX.*DirectionsX + DirectionsY.*DirectionsY);
Directions = [DirectionsX./Lengths DirectionsY./Lengths];
MidPoints = [(PosX(I)+PosX(J))/2  (PosY(I)+PosY(J))/2];
D_Inv = 1./Lengths;