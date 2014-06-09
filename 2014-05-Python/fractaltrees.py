def tree():
  #print '\n'.join(''.join(['1' if (i == posx-j or i == posx+j) else '_' for i in range(width)]) for j in range(2**(n-1)+1,1,-1))
  #print '\n'.join(2**(n-1)*[''.join(['1' if i == posx else '_' for i in range(width)])])
  print '\n'.join(''.join(['1' if 
    i == 49+16+8+4+2-j or i == 49+16+8+4+2+j or 
    i == 49+16+8+4-2-j or i == 49+16+8+4-2+j or 
    i == 49+16+8-4-2-j or i == 49+16+8-4-2+j or 
    i == 49+16-8-4-2-j or i == 49+16-8-4-2+j or 
    i == 49-16-8-4-2-j or i == 49-16-8-4-2+j or 
    i == 49-16-8-4+2-j or i == 49-16-8-4+2+j or 
    i == 49-16-8+4+2-j or i == 49-16-8+4+2+j or 
    i == 49-16+8+4+2-j or i == 49-16+8+4+2+j or 
    i == 49+16-8+4+2-j or i == 49+16-8+4+2+j or 
    i == 49+16-8+4-2-j or i == 49+16-8+4-2+j or
    i == 49+16+8-4+2-j or i == 49+16+8-4+2+j or 
    i == 49-16+8-4+2-j or i == 49-16+8-4+2+j or 
    i == 49+16-8-4+2-j or i == 49+16-8-4+2+j or 
    i == 49-16+8-4-2-j or i == 49-16+8-4-2+j or 
    i == 49-16-8+4-2-j or i == 49-16-8+4-2+j or 
    i == 49-16+8+4-2-j or i == 49-16+8+4-2+j 
    else '_' for i in range(100)]) for j in range(1,0,-1))
  print '\n'.join([''.join(['1' if 
    i == 49-16-8-4-2 or i == 49+16-8-4-2 or i == 49-16+8-4-2 or i == 49+16+8-4-2 or 
    i == 49+16+8+4-2 or i == 49-16+8+4-2 or i == 49+16-8+4-2 or i == 49-16-8+4-2 or 
    i == 49-16-8-4-2 or i == 49-16-8-4+2 or i == 49+16-8-4+2 or i == 49-16+8-4+2 or 
    i == 49+16+8-4+2 or i == 49+16+8+4+2 or i == 49-16+8+4+2 or i == 49+16-8+4+2 or 
    i == 49-16-8+4+2 or i == 49-16-8-4+2 else '_' for i in range(100)])])

  print '\n'.join(''.join(['1' if i == 49-16-8-4-j or i == 49-16-8-4+j or i == 49+16-8-4-j or i == 49+16-8-4+j or i == 49-16+8-4-j or i == 49-16+8-4+j or i == 49+16+8-4-j or i == 49+16+8-4+j or i == 49+16+8+4-j or i == 49+16+8+4+j or i == 49-16+8+4-j or i == 49-16+8+4+j or i == 49+16-8+4-j or i == 49+16-8+4+j or i == 49-16-8+4-j or i == 49-16-8+4+j else '_' for i in range(100)]) for j in range(2,0,-1))
  print '\n'.join(2*[''.join(['1' if i == 49-16-8-4 or i == 49+16-8-4 or i == 49-16+8-4 or i == 49+16+8-4 or i == 49+16+8+4 or i == 49-16+8+4 or i == 49+16-8+4 or i == 49-16-8+4 else '_' for i in range(100)])])

  print '\n'.join(''.join(['1' if i == 49-16-8-j or i == 49-16-8+j or i == 49+16-8-j or i == 49+16-8+j or i == 49-16+8-j or i == 49-16+8+j or i == 49+16+8-j or i == 49+16+8+j else '_' for i in range(100)]) for j in range(4,0,-1))
  print '\n'.join(4*[''.join(['1' if i == 49-16-8 or i == 49+16-8 or i == 49-16+8 or i == 49+16+8 else '_' for i in range(100)])])

  print '\n'.join(''.join(['1' if i == 49-16-j or i == 49-16+j or i == 49+16-j or i == 49+16+j else '_' for i in range(100)]) for j in range(8,0,-1))
  print '\n'.join(8*[''.join(['1' if i == 49-16 or i == 49+16 else '_' for i in range(100)])])

  print '\n'.join(''.join(['1' if i == 49-j or i == 49+j else '_' for i in range(100)]) for j in range(16,0,-1))
  print '\n'.join(16*[''.join(['1' if i == 49 else '_' for i in range(100)])])

#width  = 100
#height = 63
#n = 5
print 100*'_'
tree()
#[tree(width/2-1-2**(n-i)-1,i) for i in range(1,n+1)]
