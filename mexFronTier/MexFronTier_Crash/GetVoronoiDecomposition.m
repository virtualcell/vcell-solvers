function [NatNeighborPairs, V, R] =...
    GetVoronoiDecomposition(Idx, PosX, PosY, InsideMask, Candidates, Nx, Ny)

MinX = min(PosX);
MaxX = max(PosX);
MinY = min(PosY);
MaxY = max(PosY);

NumBoundaryPts = length(Idx);

% Finding natural neighbor candidates 

if isempty(Candidates)    
    [I, J]= ind2sub([Nx, Ny], Idx);
    
    Candidates_IJ = [I-2 J; I-1 J; I+1 J; I+2 J
        I J-2; I J-1; I J+1; I J+2
        I-1 J-1; I-1 J+1; I+1 J-1; I+1 J+1];
    
    Candidates_IJ = Candidates_IJ(Candidates_IJ(:,1)>0 & Candidates_IJ(:,1)<=Nx & Candidates_IJ(:,2)>0 & Candidates_IJ(:,2)<=Ny,:);
    Candidates = Candidates_IJ(:,1) + (Candidates_IJ(:,2)-1)*Nx;
    AllIdx = [Idx; setdiff(unique(Candidates(InsideMask(Candidates)>0)), Idx)];
else
    AllIdx = [Idx; setdiff(Candidates, Idx)];
end
NumAllPts = length(AllIdx);

% Include ghost points 

X = [PosX(AllIdx),PosY(AllIdx)];
MirrorUp = [PosX(Idx), 2*MaxY-PosY(Idx)]; 
MirrorDown = [PosX(Idx), 2*MinY-PosY(Idx)]; 
MirrorLeft = [2*MinX-PosX(Idx), PosY(Idx)]; 
MirrorRight = [2*MaxX-PosX(Idx), PosY(Idx)]; 

% Perform Delanay triangulation to find natural neighbors 

tri = DelaunayTri([X; MirrorUp; MirrorDown; MirrorLeft; MirrorRight]);
[V, R] = voronoiDiagram(tri);

% Discard ghost points data 

R = R(1:NumBoundaryPts);

% Create connectivity matrix 

NatNeighborPairs = [tri.Triangulation; tri.Triangulation(:,[2 3 1]); tri.Triangulation(:,[3 1 2])];
NatNeighborPairs = [NatNeighborPairs(:,2) NatNeighborPairs(:,1); NatNeighborPairs(:,3) NatNeighborPairs(:,1)];

% Filter out unnecessary info 

MaxIdx = max(NatNeighborPairs(:,1));
Tag = NatNeighborPairs(:,1)*MaxIdx+NatNeighborPairs(:,2);
[~, ind] = sort(Tag);
M = size(NatNeighborPairs,1);
NatNeighborPairs = NatNeighborPairs(ind(1:2:M),:);
NatNeighborPairs = NatNeighborPairs(NatNeighborPairs(:,2)<=NumBoundaryPts & NatNeighborPairs(:,1)<=NumAllPts,:);

% Output

NatNeighborPairs(:,1) = AllIdx(NatNeighborPairs(:,1));
NatNeighborPairs(:,2) = AllIdx(NatNeighborPairs(:,2));
