// AutoLisp - 문자 찾기/바꾸기 대화상자
// build.py 가 이 파일을 LSP 안에 넣고, 실행할 때 임시 폴더에 풀어서 사용한다.

al_fte : dialog {
  label = "AutoLisp - 문자 찾기/바꾸기";
  : row {
    : column {
      : boxed_row {
        label = "검색 위치";
        : popup_list { key = "scope"; width = 22; }
        : button { key = "selobj"; label = "객체 선택 <"; fixed_width = true; }
      }
      : boxed_column {
        label = "문자 찾기";
        : row {
          : edit_box { key = "ftext"; label = "내용"; edit_width = 26; allow_accept = true; }
          : button { key = "fpick"; label = "선택<"; fixed_width = true; }
          : button { key = "fpickm"; label = "여러 개<"; fixed_width = true; }
        }
        : row {
          : popup_list { key = "fhist"; label = "최근 내용"; width = 36; }
          : button { key = "fhclr"; label = "기록 삭제"; fixed_width = true; }
        }
        : toggle { key = "fwhole"; label = "전체 내용 일치 문자만 찾기"; }
        : toggle { key = "fmulti"; label = "찾는 내용 쉼표(,) 구분해 여러 문자 찾기"; }
        : toggle { key = "fcase"; label = "대소문자 구분"; }
        : button { key = "dofind"; label = "찾기"; is_default = true; }
      }
    }
    : boxed_column {
      label = "문자 바꾸기";
      : row {
        : edit_box { key = "rfind"; label = "찾을 문자"; edit_width = 22; }
        : button { key = "rpick"; label = "선택<"; fixed_width = true; }
      }
      : edit_box { key = "rrepl"; label = "바꿀 문자"; edit_width = 22; }
      : row {
        : button { key = "radd"; label = "목록에 추가"; }
        : button { key = "rdel"; label = "선택 삭제"; }
        : button { key = "rclr"; label = "전체 삭제"; }
      }
      : list_box { key = "rlist"; height = 6; width = 50; multiple_select = true; tabs = "24"; }
      : row {
        : button { key = "rtxt"; label = "텍스트 파일..."; }
        : edit_box { key = "rdelim"; label = "구분 문자"; edit_width = 3; }
        : button { key = "rxls"; label = "엑셀 선택 영역..."; }
      }
      : row {
        : popup_list { key = "rsaved"; label = "저장 목록"; width = 30; }
        : button { key = "rload"; label = "불러오기"; fixed_width = true; }
        : button { key = "rsdel"; label = "삭제"; fixed_width = true; }
      }
      : row {
        : edit_box { key = "rsname"; label = "저장 이름"; edit_width = 16; }
        : button { key = "rsave"; label = "현재 목록 저장"; fixed_width = true; }
      }
      : toggle { key = "rwhole"; label = "전체 내용 일치 문자만 바꾸기"; }
      : toggle { key = "rcase"; label = "대소문자 구분"; }
      : button { key = "doreplace"; label = "바꾸기 실행"; }
    }
  }
  : boxed_column {
    label = "결과 (더블 클릭하면 확대)";
    : list_box { key = "results"; height = 12; width = 110; multiple_select = true; tabs = "6 16 36"; }
    : text { key = "status"; width = 100; }
    : row {
      : button { key = "rzoom"; label = "확대"; }
      : button { key = "rsel"; label = "선택 항목 도면에서 선택"; }
      : button { key = "rselall"; label = "찾은 문자 모두 선택"; }
      : button { key = "rline"; label = "연결선 그리기"; }
      : button { key = "rerase"; label = "선택 항목 삭제"; }
      : button { key = "rremove"; label = "목록에서 제거"; }
    }
  }
  : row {
    : button { key = "settings"; label = "검색 설정 / 찾은 문자 처리..."; fixed_width = true; }
    : text { key = "procinfo"; width = 46; }
    : button { key = "cancel"; label = "닫기"; is_cancel = true; fixed_width = true; }
  }
}

al_fte_set : dialog {
  label = "검색 설정";
  : row {
    : boxed_column {
      label = "검색 대상 객체";
      : toggle { key = "t_text"; label = "단일행 문자 (TEXT)"; }
      : toggle { key = "t_mtext"; label = "여러 줄 문자 (MTEXT)"; }
      : toggle { key = "t_attr"; label = "블록 속성, 속성 정의"; }
      : toggle { key = "t_block"; label = "블록 내부 문자"; }
      : toggle { key = "t_dim"; label = "치수 (재지정 문자만)"; }
      : toggle { key = "t_mlead"; label = "다중 지시선"; }
      : toggle { key = "t_table"; label = "표 (테이블)"; }
    }
    : boxed_column {
      label = "옵션";
      : toggle { key = "oneblock"; label = "동일 블록은 결과에 항목 하나로 표기"; }
      : toggle { key = "fspace"; label = "찾기 띄어쓰기 무시"; }
      : toggle { key = "rspace"; label = "바꾸기 띄어쓰기 무시 (바꾼 문자는 띄어쓰기 삭제됨)"; }
      : toggle { key = "foff"; label = "찾기 꺼진/동결 레이어 제외"; }
      : toggle { key = "roff"; label = "바꾸기 꺼진/동결 레이어 제외"; }
      : toggle { key = "wild"; label = "와일드카드 사용  ( *  #  {*} )"; }
      : edit_box { key = "zoom"; label = "찾은 문자 Zoom 단계 (작을수록 크게)"; edit_width = 6; }
      : toggle { key = "rect"; label = "Zoom 할 때 문자 영역에 가상 사각형 표시"; }
    }
  }
  : boxed_column {
    label = "찾은 문자 처리 (찾기/바꾸기 공통)";
    : radio_row {
      key = "proc";
      : radio_button { key = "p0"; label = "처리 없음"; }
      : radio_button { key = "p1"; label = "찾은 문자 삭제"; }
      : radio_button { key = "p2"; label = "찾은 문자 특성 변경"; }
    }
    : row {
      : toggle { key = "ph"; label = "높이"; }
      : edit_box { key = "phv"; edit_width = 8; }
      : toggle { key = "pw"; label = "폭 비율"; }
      : edit_box { key = "pwv"; edit_width = 8; }
      : toggle { key = "pc"; label = "색상 (0~256)"; }
      : edit_box { key = "pcv"; edit_width = 5; }
    }
    : row {
      : toggle { key = "ps"; label = "문자 스타일"; }
      : popup_list { key = "psv"; width = 22; }
      : toggle { key = "pl"; label = "레이어"; }
      : popup_list { key = "plv"; width = 26; }
    }
  }
  ok_cancel;
}
