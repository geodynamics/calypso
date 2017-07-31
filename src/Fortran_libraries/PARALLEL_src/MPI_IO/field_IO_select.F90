!> @file  field_IO_select.F90
!!      module field_IO_select
!!
!!@author  H. Matsui
!!@date Programmed in July, 2006
!!@n    Modified in May, 2009
!!@n    Modified in June, 2015
!
!> @brief read and write restart file
!!
!!@verbatim
!!      integer(kind = kint) function check_step_FEM_field_file         &
!!     &                            (id_rank, istep_fld, file_IO)
!!
!!      subroutine sel_write_step_FEM_field_file                        &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!      subroutine sel_write_step_SPH_field_file                        &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!
!!      subroutine sel_read_step_FEM_field_file                         &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!      subroutine sel_read_step_SPH_field_file                         &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!
!!      subroutine sel_read_alloc_step_FEM_file                         &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!      subroutine sel_read_alloc_step_SPH_file                         &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!
!!      subroutine sel_read_alloc_FEM_fld_head                          &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!      subroutine sel_read_alloc_SPH_fld_head                          &
!!     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!!        type(field_IO_params), intent(in) :: file_IO
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module field_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use field_file_IO
      use field_file_IO_b
      use field_file_MPI_IO
      use field_file_MPI_IO_b
!
      use t_file_IO_parameter
      use t_time_data
      use t_field_data_IO
!
#ifdef ZLIB_IO
      use gz_field_file_IO
      use gz_field_file_IO_b
      use gz_field_file_MPI_IO
      use gz_field_file_MPI_IO_b
#endif
!
      implicit none
!
      private :: sel_write_step_field_file, sel_read_step_field_file
      private :: sel_read_alloc_step_field_file
      private :: sel_read_alloc_field_head
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_step_FEM_field_file           &
     &                            (id_rank, istep_fld, file_IO)
!
      use set_field_file_names
      use delete_data_files
!
      integer(kind=kint), intent(in) :: id_rank, istep_fld
      type(field_IO_params), intent(in) :: file_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      check_step_FEM_field_file = check_file_exist(file_name)
!
      end function check_step_FEM_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_step_FEM_field_file                          &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: nprocs_in, id_rank, istep_fld
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_write_step_field_file                                    &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_write_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_write_step_SPH_field_file                          &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: nprocs_in, id_rank, istep_fld
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_write_step_field_file                                    &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_write_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_step_FEM_field_file                           &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: id_rank, istep_fld, nprocs_in
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_read_step_field_file                                     &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_read_step_FEM_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_step_SPH_field_file                           &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: id_rank, istep_fld, nprocs_in
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_read_step_field_file                                     &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_read_step_SPH_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_FEM_file                           &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: id_rank, istep_fld, nprocs_in
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_read_alloc_step_field_file                               &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_read_alloc_step_FEM_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_SPH_file                           &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: id_rank, istep_fld, nprocs_in
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_read_alloc_step_field_file                               &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_read_alloc_step_SPH_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_FEM_fld_head                            &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: id_rank, istep_fld, nprocs_in
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_FEM_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_read_alloc_field_head                                    &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_read_alloc_FEM_fld_head
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_SPH_fld_head                            &
     &         (nprocs_in, id_rank, istep_fld, file_IO, t_IO, fld_IO)
!
      use set_field_file_names
!
      integer(kind=kint), intent(in) :: id_rank, istep_fld, nprocs_in
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      character(len=kchara) :: file_name
!
!
      call set_SPH_fld_file_name(file_IO%file_prefix,                   &
     &    file_IO%iflag_format, id_rank, istep_fld, file_name)
!
      call sel_read_alloc_field_head                                    &
     &    (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      end subroutine sel_read_alloc_SPH_fld_head
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_step_field_file                              &
     &     (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      use calypso_mpi
      use m_error_IDs
!
      character(len=kchara), intent(in) :: file_name
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      if( (file_IO%iflag_format/iflag_single) .eq. 0) then
        if(id_rank .ge. nprocs_in)  return
      end if
!
      if(file_IO%iflag_format .eq. iflag_single) then
        call write_step_field_file_mpi                                  &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_binary_file_fmt) then
        call write_step_field_file_mpi_b                                &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
#ifdef ZLIB_IO
      else if(file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_step_field_file_b(file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_write_step_fld_file_mpi_b                               &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_txt_file_fmt) then
        if(nprocs .eq. nprocs_in) then
          call write_gz_step_field_file_mpi                             &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
        else
          call calypso_mpi_abort                                        &
     &      (ierr_fld, 'gzipped data output does not dort')
        end if
      else if(file_IO%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_write_step_fld_file_b(file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format .eq. id_gzip_txt_file_fmt) then
        call write_gz_step_field_file(file_name, id_rank, t_IO, fld_IO)
#endif
!
      else
        call write_step_field_file(file_name, id_rank, t_IO, fld_IO)
      end if
!
      end subroutine sel_write_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_step_field_file                               &
     &     (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      integer(kind = kint), intent(in) :: id_rank, nprocs_in
      character(len=kchara), intent(in) :: file_name
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      if( (file_IO%iflag_format/iflag_single) .eq. 0) then
        if(id_rank .ge. nprocs_in) return
      end if
!
      if(file_IO%iflag_format .eq. iflag_single) then
        call read_step_field_file_mpi                                   &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_binary_file_fmt) then
        call read_step_field_file_mpi_b                                 &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
#ifdef ZLIB_IO
      else if (file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_step_field_file_b(file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_read_step_field_file_mpi_b                              &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_txt_file_fmt) then
        call read_step_field_file_gz_mpi                                &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_step_field_file_b                                  &
     &     (file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format .eq. id_gzip_txt_file_fmt) then
        call read_gz_step_field_file(file_name, id_rank, t_IO, fld_IO)
#endif
!
      else
        call read_step_field_file(file_name, id_rank, t_IO, fld_IO)
      end if
!
      end subroutine sel_read_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_step_field_file                         &
     &         (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      integer(kind = kint), intent(in) :: id_rank, nprocs_in
      character(len=kchara), intent(in) :: file_name
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      if( (file_IO%iflag_format/iflag_single) .eq. 0) then
        if(id_rank .ge. nprocs_in) return
      end if
!
      if(file_IO%iflag_format .eq. iflag_single) then
        call read_alloc_step_fld_file_mpi                               &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_binary_file_fmt) then
        call read_alloc_stp_fld_file_mpi_b                              &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
#ifdef ZLIB_IO
      else if (file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_and_allocate_step_field_b                             &
     &     (file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_rd_alloc_st_fld_file_mpi_b                              &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_txt_file_fmt) then
        call read_alloc_stp_fld_file_gz_mpi                             &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_rd_alloc_st_fld_file_b                                  &
     &     (file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_step_field_file                              &
     &     (file_name, id_rank, t_IO, fld_IO)
#endif
!
      else
        call read_and_alloc_step_field                                  &
     &     (file_name, id_rank, t_IO, fld_IO)
      end if
!
      end subroutine sel_read_alloc_step_field_file
!
!------------------------------------------------------------------
!
      subroutine sel_read_alloc_field_head                              &
     &         (file_name, nprocs_in, id_rank, file_IO, t_IO, fld_IO)
!
      integer(kind = kint), intent(in) :: id_rank, nprocs_in
      character(len=kchara), intent(in) :: file_name
      type(field_IO_params), intent(in) :: file_IO
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      if( (file_IO%iflag_format/iflag_single) .eq. 0) then
        if(id_rank .ge. nprocs_in) return
      end if
!
      if(file_IO%iflag_format .eq. iflag_single) then
        call read_alloc_step_fld_head_mpi                               &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_binary_file_fmt) then
        call read_alloc_stp_fld_head_mpi_b                              &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
!
#ifdef ZLIB_IO
      else if (file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_and_allocate_step_head_b                              &
     &     (file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_rd_alloc_st_fld_head_mpi_b                              &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format                                      &
     &       .eq. iflag_single+id_gzip_txt_file_fmt) then
        call read_alloc_stp_fld_head_gz_mpi                             &
     &     (file_name, nprocs_in, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_rd_alloc_st_fld_head_b                                  &
     &     (file_name, id_rank, t_IO, fld_IO)
      else if(file_IO%iflag_format .eq. id_gzip_txt_file_fmt) then
        call read_alloc_gz_step_field_head                              &
     &     (file_name, id_rank, t_IO, fld_IO)
#endif
!
      else
        call read_and_allocate_step_head                                &
     &     (file_name, id_rank, t_IO, fld_IO)
      end if
!
      end subroutine sel_read_alloc_field_head
!
!------------------------------------------------------------------
!
      end module field_IO_select
