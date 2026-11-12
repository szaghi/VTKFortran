error = amr%initialize(filename='ref.vthb', composite='OverlappingAMR')
error = amr%write_level(level=0, filenames=['ref_amr_0.vti'])
error = amr%write_level(level=1, filenames=['ref_amr_1.vti', 'ref_amr_2.vti'])
error = amr%finalize()
