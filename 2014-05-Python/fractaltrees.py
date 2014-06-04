def tree(posx,n):
  print '\n'.join(''.join(['1' if (i == posx-j or i == posx+j) else '_' for i in range(width)]) for j in range(2**(n-1)+1,1,-1))
  print '\n'.join(2**(n-1)*[''.join(['1' if i == posx else '_' for i in range(width)])])

width  = 100
height = 63
n = 5
print 100*'_'
[tree(width/2-1-2**(n-i)-1,i) for i in range(1,n+1)]
