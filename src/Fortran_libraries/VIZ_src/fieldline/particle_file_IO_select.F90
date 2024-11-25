!>@file   particle_file_IO_select.f90
!!@brief  module particle_file_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_read_particle_file                               &
!!     &         (file_prm, id_rank, t_IO, particle_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        type(field_IO_params), intent(in) ::  file_prm
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine sel_write_particle_file                              &
!!     &         (file_prm, id_rank, t_IO, particle_IO)
!!        integer, intent(in) :: id_rank
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!@endverbatim
!
      module particle_file_IO_select
!
      use m_precision
!
      use t_file_IO_parameter
      use t_mesh_data
      use m_file_format_switch
!
      use mesh_file_name_by_param
      use particle_file_IO
      use particle_file_IO_b
#ifdef ZLIB_IO
      use gz_particle_file_IO
      use gz_particle_file_IO_b
#endif
!
      implicit none
!
      character(len=3), parameter, private :: pcl_ext = "pcl"
      character(len=3), parameter, private :: pcb_ext = "pcb"
!
      private :: add_pcl_extension, add_pcb_extension
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      character(len=kchara) function add_pcl_extension(file_head)
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_pcl_extension = add_3chara_extension(file_head, pcl_ext)
!
      end function add_pcl_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_pcb_extension(file_head)
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_pcb_extension = add_3chara_extension(file_head, pcb_ext)
!
      end function add_pcb_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function set_tracer_file_name               &
     &                    (file_prefix, itype_file, id_rank, istep_fld)
!
      use set_parallel_file_name
      use set_sph_extensions
      use m_file_format_switch
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file, istep_fld
      character(len=kchara), intent(in) ::    file_prefix
      character(len=kchara) :: fname_tmp, file_name
!
!
      if(istep_fld .eq. iminus) then
        fname_tmp = add_elaps_postfix(file_prefix)
      else
        fname_tmp = add_int_suffix(istep_fld, file_prefix)
      end if
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, fname_tmp)
      else
        file_name = fname_tmp
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_pcb_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_pcl_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_pcb_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp =  add_pcl_extension(file_name)
        file_name = fname_tmp
      end if
      set_tracer_file_name = file_name
!
      end function set_tracer_file_name
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_particle_file(file_prm, id_rank, istep_file,  &
     &                                  t_IO, particle_IO, ierr)
!
      use set_element_mesh_file_names
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_file
      type(field_IO_params), intent(in) ::  file_prm
!
      type(time_data), intent(inout) :: t_IO
      type(surf_edge_IO_file), intent(inout) :: particle_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
      file_name = set_tracer_file_name(file_prm%file_prefix,            &
     &                                 file_prm%iflag_format,           &
     &                                 id_rank, istep_file)
!
      if (file_prm%iflag_format .eq. id_binary_file_fmt) then
        call read_particle_file_b(id_rank, file_name,                   &
     &                            t_IO, particle_IO, ierr)
!
#ifdef ZLIB_IO
      else if(file_prm%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_particle_file_b(id_rank, file_name,                &
     &                               t_IO, particle_IO, ierr)
      else if(file_prm%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_read_particle_file(id_rank, file_name,                  &
     &                             t_IO, particle_IO, ierr)
#endif
!
      else
        call read_particle_file(id_rank, file_name,                     &
     &                          t_IO, particle_IO, ierr)
      end if 
!
      end subroutine sel_read_particle_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_particle_file                                &
     &         (file_prm, id_rank, istep_file, t_IO, particle_IO)
!
      use set_element_mesh_file_names
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: istep_file
      type(field_IO_params), intent(in) ::  file_prm
!
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
      integer(kind = kint) :: ierr = 0
!
      character(len=kchara) :: file_name
!
      file_name = set_tracer_file_name(file_prm%file_prefix,            &
     &                                 file_prm%iflag_format,           &
     &                                 id_rank, istep_file)
!
      if (file_prm%iflag_format .eq. id_binary_file_fmt) then
        call write_particle_file_b(id_rank, file_name,                  &
     &                             t_IO, particle_IO, ierr)
!
#ifdef ZLIB_IO
      else if(file_prm%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_write_particle_file_b(id_rank, file_name,               &
     &                                t_IO, particle_IO)
      else if(file_prm%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_write_particle_file(id_rank, file_name,                 &
     &                              t_IO, particle_IO)
#endif
!
      else
        call write_particle_file(id_rank, file_name, t_IO, particle_IO)
      end if
!
      end subroutine sel_write_particle_file
!
!  ---------------------------------------------------------------------
!
      end module particle_file_IO_select
