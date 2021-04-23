!>@file   t_ctl_data_FEM_sleeve_size.f90
!!        module t_ctl_data_FEM_sleeve_size
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine read_FEM_sleeve_control                              &
!!     &         (id_control, hd_block, sleeve_ctl, c_buf)
!!        type(FEM_sleeve_control), intent(inout) :: sleeve_ctl
!!      subroutine write_FEM_sleeve_control                             &
!!     &         (id_file, hd_block, sleeve_ctl, level)
!!      subroutine dealloc_ctl_data_FEM_sleeve(sleeve_ctl)
!!
!! ------------------------------------------------------------------
!!      Example of control parameters
!!
!!    begin FEM_sleeve_ctl
!!!!         sleeve_extension_mode: 
!!!!             element_count, sleeve_length, vector_trace
!!      sleeve_extension_mode        element_count
!!      sleeve_level_ctl              2
!!      sleeve_size_ctl               2
!!      array reference_vector_ctl
!!        reference_vector_ctl        magnetic_field
!!      end array reference_vector_ctl
!!    end FEM_sleeve_ctl
!! ------------------------------------------------------------------
!!@endverbatim
!!
      module t_ctl_data_FEM_sleeve_size
!
      use m_precision
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_real
!
      implicit  none
!
!
!>      Structure of Sleeve size controls
      type FEM_sleeve_control
!>        Structure of Sleeve extension mode
        type(read_character_item) :: sleeve_extension_mode_ctl
!>        Structure of number of sleeve level
        type(read_integer_item) ::   sleeve_level_ctl
!>        Structure of number of sleeve size in hysocal space
        type(read_real_item) ::      sleeve_size_ctl
!>        Structure of reference vector field for sleeve extension
        type(ctl_array_chara) ::     ref_vector_ctl
!
        integer(kind=kint) :: i_FEM_sleeve_ctl =   0
      end type FEM_sleeve_control
!
!   file and domain controls
!
      character(len=kchara), parameter, private                         &
     &       :: hd_sleeve_extension_mode =   'sleeve_extension_mode'
      character(len=kchara), parameter, private                         &
     &       :: hd_sleeve_level =            'sleeve_level_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_sleeve_size =             'sleeve_size_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_reference_vector =        'reference_vector_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_FEM_sleeve_control                                &
     &         (id_control, hd_block, sleeve_ctl, c_buf)
!
      use m_machine_parameter
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(FEM_sleeve_control), intent(inout) :: sleeve_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(sleeve_ctl%i_FEM_sleeve_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sleeve_level, sleeve_ctl%sleeve_level_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_sleeve_size,  sleeve_ctl%sleeve_size_ctl)
!
        call read_chara_ctl_type(c_buf, hd_sleeve_extension_mode,       &
     &      sleeve_ctl%sleeve_extension_mode_ctl)
!
        call read_control_array_c1(id_control, hd_reference_vector,     &
     &      sleeve_ctl%ref_vector_ctl, c_buf)
       end do
       sleeve_ctl%i_FEM_sleeve_ctl = 1
!
      end subroutine read_FEM_sleeve_control
!
!  ---------------------------------------------------------------------
!
      subroutine write_FEM_sleeve_control                               &
     &         (id_file, hd_block, sleeve_ctl, level)
!
      use m_machine_parameter
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: hd_block
      type(FEM_sleeve_control), intent(in) :: sleeve_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
      maxlen = max(maxlen, len_trim(hd_sleeve_extension_mode))
      maxlen = max(maxlen, len_trim(hd_sleeve_level))
      maxlen = max(maxlen, len_trim(hd_sleeve_size))
      maxlen = max(maxlen, len_trim(hd_reference_vector))
!
      write(id_file,'(a)') '!'
!
      call write_chara_ctl_type                                         &
     &   (id_file, level, maxlen, hd_sleeve_extension_mode,             &
     &    sleeve_ctl%sleeve_extension_mode_ctl)
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_sleeve_level, sleeve_ctl%sleeve_level_ctl)
      call write_real_ctl_type(id_file, level, maxlen,                  &
     &    hd_sleeve_size, sleeve_ctl%sleeve_size_ctl)
      call write_control_array_c1(id_file, level, hd_reference_vector,  &
     &    sleeve_ctl%ref_vector_ctl)
!
      level =  write_end_flag_for_ctl(id_file, level, hd_block)
!
      end subroutine write_FEM_sleeve_control
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_FEM_sleeve(sleeve_ctl)
!
      type(FEM_sleeve_control), intent(inout) :: sleeve_ctl
!
!
      call dealloc_control_array_chara(sleeve_ctl%ref_vector_ctl)
      sleeve_ctl%sleeve_size_ctl%iflag =      0
      sleeve_ctl%sleeve_level_ctl%iflag =      0
      sleeve_ctl%sleeve_extension_mode_ctl%iflag =  0
!
      sleeve_ctl%i_FEM_sleeve_ctl = 0
!
      end subroutine dealloc_ctl_data_FEM_sleeve
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_FEM_sleeve_size
