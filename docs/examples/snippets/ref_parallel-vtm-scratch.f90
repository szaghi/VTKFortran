error = a_vtm_file%initialize(filename='ref_series.vtm', scratch_units_number=2)       ! 2 blocks
error = a_vtm_file%write_block(scratch=1, action='open', name='fluid domain')         ! the name of each block
error = a_vtm_file%write_block(scratch=2, action='open', name='solid')
do step=1, 3   ! the files of the blocks are known one at a time, interleaved
  error = a_vtm_file%write_block(scratch=1, action='write', filename='fluid/step_'//trim(str(step, .true.))//'.vtu')
  error = a_vtm_file%write_block(scratch=2, action='write', filename='solid/step_'//trim(str(step, .true.))//'.vtu')
enddo
error = a_vtm_file%finalize()                                                         ! writes the blocks
