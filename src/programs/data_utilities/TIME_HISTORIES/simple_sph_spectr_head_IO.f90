!>@file   simple_sph_spectr_head_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Header output for spectrum data
!!
!!@verbatim
!!      subroutine read_sph_pwr_vol_head(id_file, sph_IN)
!!      subroutine read_sph_pwr_layer_head(id_file, sph_IN)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!      subroutine read_sph_spectr_name                                 &
!!     &         (id_file, nfield_sph_spec, num_labels,                 &
!!     &          ncomp_sph_spec, ene_sph_spec_name)
!!
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nfield_sph_spec, num_labels
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: ncomp_sph_spec(nfield_sph_spec)
!!        character(len = kchara), intent(inout)                        &
!!     &                     :: ene_sph_spec_name(num_labels)
!!
!!      subroutine write_sph_pwr_vol_head(id_file, sph_IN)
!!      subroutine write_sph_pwr_layer_head(id_file, sph_IN)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!@endverbatim
!
      module simple_sph_spectr_head_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_pwr_vol_head(id_file, sph_IN)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nri_sph, sph_IN%ltr_sph
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_inner, sph_IN%r_inner
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_outer, sph_IN%r_outer
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
!
      end subroutine read_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_pwr_layer_head(id_file, sph_IN)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nri_sph, sph_IN%ltr_sph
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call skip_comment(character_4_read, id_file)
      read(id_file,*) sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
!
      end subroutine read_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_vol_head(id_file, sph_IN)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') sph_IN%nri_sph, sph_IN%ltr_sph
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') sph_IN%kr_ICB, sph_IN%kr_CMB
      write(id_file,'(a)')    'Lower boudary'
      write(id_file,'(i16,1pe23.14e3)') sph_IN%kr_inner, sph_IN%r_inner
      write(id_file,'(a)')    'Upper boundary'
      write(id_file,'(i16,1pe23.14e3)') sph_IN%kr_outer, sph_IN%r_outer
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')                                           &
     &      sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
      write(id_file,'(16i5)')                                           &
     &      sph_IN%ncomp_sph_spec(1:sph_IN%nfield_sph_spec)
!
      do i = 1, sph_IN%num_labels
        write(id_file,'(2a)',advance='no')                              &
     &            trim(sph_IN%ene_sph_spec_name(i)), '    '
      end  do
      write(id_file,*)
!
      end subroutine write_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_layer_head(id_file, sph_IN)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') sph_IN%nri_sph, sph_IN%ltr_sph
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') sph_IN%kr_ICB, sph_IN%kr_CMB
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')                                           &
     &      sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
      write(id_file,'(16i5)')                                           &
     &      sph_IN%ncomp_sph_spec(1:sph_IN%nfield_sph_spec)
!
      do i = 1, sph_IN%num_labels
        write(id_file,'(2a)',advance='no')                              &
     &            trim(sph_IN%ene_sph_spec_name(i)), '    '
      end  do
      write(id_file,*)
!
      end subroutine write_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_spectr_name                                   &
     &         (id_file, nfield_sph_spec, num_labels,                   &
     &          ncomp_sph_spec, ene_sph_spec_name)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nfield_sph_spec, num_labels
!
      integer(kind = kint), intent(inout)                               &
     &                     :: ncomp_sph_spec(nfield_sph_spec)
      character(len = kchara), intent(inout)                            &
     &                     :: ene_sph_spec_name(num_labels)
!
      read(id_file,*) ncomp_sph_spec(1:nfield_sph_spec)
      read(id_file,*) ene_sph_spec_name(1:num_labels)

      end subroutine read_sph_spectr_name
!
!   --------------------------------------------------------------------
!
      end module simple_sph_spectr_head_IO
