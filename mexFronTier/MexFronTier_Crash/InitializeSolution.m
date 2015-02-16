function [Ux, Uy, M, InsideMask, InteriorMask, BoundaryMask] = InitializeSolution(PosX, PosY, Front, InitCondition, NumNodesX)

[InsideMask, InteriorMask, BoundaryMask] = IdentifyNodeTypes(PosX, PosY, Front, NumNodesX);

Ux = InitCondition.Ux(PosX,PosY).*InsideMask;
Uy = InitCondition.Uy(PosX,PosY).*InsideMask;
M = InitCondition.M(PosX,PosY).*InsideMask;


