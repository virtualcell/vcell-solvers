clear all
close all
clc

n=50;% number of random polygon vertices
scale=2^15;% scale factor from double to int64 (arbitrary)

p1=rand(n,2);% make random polygon (self-intersecting)
% recast into int64 in order to input to clipper
poly1.x=int64(p1(:,1)*scale);
poly1.y=int64(p1(:,2)*scale);

fill(p1(:,1),p1(:,2),'c'), hold on% plot first polygon

p2=rand(n,2);% make random polygon (self-intersecting)
% recast into int64 in order to input to clipper
poly2.x=int64(p2(:,1)*scale);
poly2.y=int64(p2(:,2)*scale);

fill(p2(:,1),p2(:,2),'y')% plot second polygon

out=clipper(poly1,poly2,1);% perform intersection

for i=1:length(out)
    fill(out(i).x/scale,out(i).y/scale,'r')% plot each output polygon
    % convert double output back to int64 for more input
    out(i).x=int64(out(i).x);
    out(i).y=int64(out(i).y);
end

offset=clipper(out,.02*scale,1);% perform outsetting

for i=1:length(offset)
    % plot output, remembering to connect last vertex to first
    plot([offset(i).x;offset(i).x(1)]/scale,...
        [offset(i).y;offset(i).y(1)]/scale,'k','linewidth',2)
    % convert double output back to int64 for more input
    offset(i).x=int64(offset(i).x);
    offset(i).y=int64(offset(i).y);
end

orientation=clipper(offset)% output orientations to screen