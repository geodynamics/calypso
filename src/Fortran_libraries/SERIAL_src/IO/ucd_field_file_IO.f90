!ucd_field_file_IO.f90
!      module ucd_field_file_IO
!
!     Written by H. Matsui
!
!      subroutine write_ucd_2_fld_file(my_rank, istep)
!
!      subroutine read_ucd_2_fld_file(my_rank, istep)
!      subroutine read_alloc_ucd_2_fld_file(my_rank, istep)
!
      module ucd_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_time_data_IO
      use m_ucd_data
      use m_field_file_format
      use field_data_IO
      use skip_gz_comment
      use set_ucd_file_names
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_fld_file(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_fld,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write ascii step data file: ', trim(file_name)
!
      open(ucd_file_code, file = file_name, form = 'formatted')
!
      call write_step_data(ucd_file_code, my_rank)
      call write_field_data(ucd_file_code,                              &
     &          nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      close (ucd_file_code)
!
      end subroutine write_ucd_2_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_fld_file(my_rank, istep)
!
      use skip_comment_f
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
      character(len=255) :: character_4_read
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_fld,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read ascii data file: ', trim(file_name)
!
      open(ucd_file_code, file = file_name, form = 'formatted')
!
      call read_step_data(ucd_file_code)
!
      call skip_comment(character_4_read, ucd_file_code)
      read(character_4_read,*) nnod_ucd, num_field_ucd
      read(ucd_file_code,*) num_comp_ucd(1:num_field_ucd)
!
      call read_field_data(ucd_file_code,                               &
     &          nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      close (ucd_file_code)
!
      end subroutine read_ucd_2_fld_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_fld_file(my_rank, istep)
!
      use skip_comment_f
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: file_name
      character(len=255) :: character_4_read
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_fld,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read ascii data file: ', trim(file_name)
!
      open(ucd_file_code, file = file_name, form = 'formatted')
!
      call read_step_data(ucd_file_code)
!
      call skip_comment(character_4_read, ucd_file_code)
      read(character_4_read,*) nnod_ucd, num_field_ucd
!
      call allocate_ucd_phys_name
      read(ucd_file_code,*) num_comp_ucd(1:num_field_ucd)
!
      call cal_istack_ucd_component
      call allocate_ucd_phys_data
!
      call read_field_data(ucd_file_code,                               &
     &          nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      close (ucd_file_code)
!
      end subroutine read_alloc_ucd_2_fld_file
!
!------------------------------------------------------------------
!
      end module ucd_field_file_IO
