      program psf_file_to_VTK
!
      use m_precision
      use m_constants
      use m_field_file_format
      use m_section_file_extensions
!
      use t_file_IO_parameter
      use t_ucd_data
      use ucd_IO_select
!
      implicit none
!
      integer(kind = kint) :: istep, istart, iend, increment
      integer(kind = kint) :: ifmt_input = iflag_vtk
      type(field_IO_params) :: ucd_param
      type(time_data), save :: t_IO
      type(ucd_data), save :: ucd
!
!
      ucd_param%iflag_IO = 0
      write(*,*) 'Input file prefix'
      read(*,*) ucd_param%file_prefix
!
  90  continue
      ifmt_input = psf_to_vtk_format_id_from_input()
      write(*,*) 'ifmt_input', ifmt_input
      if(ifmt_input .eq. iflag_vtk) then
        write(*,*) 'Set correct file extension (except for vtk)'
        go to 90
      end if
!
      write(*,*) 'Input start, end, and increment of file step'
      read(*,*) istart, iend, increment
!
      do istep = istart, iend, increment
        ucd_param%iflag_format = ifmt_input
        call sel_read_alloc_ucd_file(-1, istep, ucd_param, ucd)
!
        ucd_param%iflag_format = iflag_vtk
        call sel_write_ucd_file(-1, istep, ucd_param, t_IO, ucd)
        call deallocate_ucd_mesh(ucd)
      end do
!
      end program psf_file_to_VTK
