error = reader%initialize(filename='quadtree.vtkhdf', action='read')
error = reader%xml_reader%get_trees(tree_ids=ids, branch_factor=bf)
print '(A,I0,A,I0)', 'read back: ', size(ids), ' trees, branch factor ', bf
do t=1, size(ids) ! each tree as write_tree took it
  error = reader%xml_reader%read_tree(tree_id=ids(t), descriptor=descriptor)
  print '(A,I0,A,I0,A,I0,A)', '  tree ', ids(t), ': ', size(descriptor), ' descriptor bits, ', count(descriptor), ' refined'
enddo
error = reader%xml_reader%read_dataarray(location='cell', data_name='depth', x=depths_read)
print '(A,L1)', '  the cell data as written: ', all(depths_read == depths)
error = reader%finalize()
