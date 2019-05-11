import styled from "styled-components";

export const Top = styled.header`
  display: flex;
  justify-content: space-between;

  > div {
    display: flex;
    flex: 1;
    justify-content: center;
    padding: 0 1em;

    &:first-child {
      justify-content: flex-start;
    }

    &:last-child {
      flex-direction: column;
      align-items: flex-end;
      justify-content: flex-end;
    }
  }
`;
