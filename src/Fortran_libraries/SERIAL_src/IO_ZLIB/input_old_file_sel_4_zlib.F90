!> @file  input_old_file_sel_4_zlib.f90
!!      module input_old_file_sel_4_zlib
!!
!! @author  H. Matsui
!! @date Programmed in July, 2006
!!@n     Modified in May, 2009
!!@n     Modified in May, 2015
!
!> @brief Selector for old restart data reading
!!
!!@verbatim
!!      subroutine sel_read_alloc_field_file                            &
!!     &         (id_rank, istep_fld, file_IO, fld_IO)
!!
!!      subroutine sel_read_rst_file                                    &
!!     &         (id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!      subroutine sel_read_rst_comps                                   &
!!     &         (id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!        type(field_IO_params), intent(in) :: file_IO
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module input_old_file_sel_4_zlib
!
      use m_precision
!
      use m_file_format_switch
      use t_file_IO_parameter
      use t_time_data
      use t_field_data_IO
!
#ifdef ZLIB_IO
      use gz_rst_data_IO_by_fld
      use gz_field_file_IO
#endif
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_field_file                              &
     &         (id_rank, istep_fld, file_IO, fld_IO)
!
      use field_file_IO
      use set_field_file_names
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_fld
      type(field_IO_params), intent(in) :: file_IO
      type(field_IO), intent(inout) :: fld_IO
      character(len=kchara) :: file_name
!
!
      file_name = set_FEM_fld_file_name(file_IO%file_prefix,            &
     &         file_IO%iflag_format, id_rank, istep_fld)
!
#ifdef ZLIB_IO
      if(file_IO%iflag_format .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_field_file                                   &
     &     (file_name, id_rank, fld_IO)
        return
      end if
#endif
!
      call read_and_allocate_field_file(file_name, id_rank, fld_IO)
!
      end subroutine sel_read_alloc_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_rst_file                                      &
     &         (id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use rst_data_IO_by_fld
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_fld
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name, fname_tmp
      integer(kind = kint) :: ierr_IO = 0
!
!
      if(istep_fld .lt. 0) then
        fname_tmp = add_elaps_postfix(file_IO%file_prefix)
      else
        fname_tmp = add_int_suffix(istep_fld, file_IO%file_prefix)
      end if
      file_name = add_process_id(id_rank, fname_tmp)
!
!
#ifdef ZLIB_IO
      if(file_IO%iflag_format .eq. id_gzip_txt_file_fmt) then
        call read_gz_rst_file                                           &
     &      (id_rank, file_name, t_IO, fld_IO, ierr_IO)
        return
      end if
#endif
!
      call read_rst_file(id_rank, file_name, t_IO, fld_IO)
!
!
      end subroutine sel_read_rst_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_rst_comps                                     &
     &         (id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use rst_data_IO_by_fld
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_fld
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name, fname_tmp
      integer(kind=kint) :: ierr
!
!
      if(istep_fld .lt. 0) then
        fname_tmp = add_elaps_postfix(file_IO%file_prefix)
      else
        fname_tmp = add_int_suffix(istep_fld, file_IO%file_prefix)
      end if
      file_name = add_process_id(id_rank, fname_tmp)
!
#ifdef ZLIB_IO
      if(file_IO%iflag_format .eq. id_gzip_txt_file_fmt) then
        call read_gz_rst_comps(id_rank, file_name, t_IO, fld_IO, ierr)
        return
      end if
#endif
!
      call read_rst_data_comps(id_rank, file_name, t_IO, fld_IO)
!
      end subroutine sel_read_rst_comps
!
!------------------------------------------------------------------
!
      end module input_old_file_sel_4_zlib
