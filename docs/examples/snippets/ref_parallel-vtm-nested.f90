error = a_vtm_file%initialize(filename='ref_machine.vtm')
error = a_vtm_file%write_block(action='open', name='assembly')                                  ! Block 0
error = a_vtm_file%write_block(filenames=['a.vtu', 'b.vtu'], names=['a', 'b'], action='write')   !   DataSet 0, 1
error = a_vtm_file%write_block(filenames=['b1.vtu', 'b2.vtu'], name='bolts')                     !   Block 2: DataSet 0, 1
error = a_vtm_file%write_block(action='open', name='sub-assembly')                              !   Block 3
error = a_vtm_file%write_block(filenames=['n.vtu'], name='nuts')                                 !     Block 0: DataSet 0
error = a_vtm_file%write_block(action='close')                                                  !   (end of Block 3)
error = a_vtm_file%write_block(action='close')                                                  ! (end of Block 0)
error = a_vtm_file%finalize()
