import React from "react";
import { Link } from "react-router-dom";
import { slide as Menu } from "react-burger-menu";
import { decorator } from "redux-burger-menu";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faChevronLeft } from "@fortawesome/free-solid-svg-icons";
import "./index.css";

import styled from "styled-components";

const BackWrap = styled.div`
  position: fixed;
  width: 36px;
  height: 30px;
  right: 36px;
  top: 36px;
  font-size: 2em;
  cursor: pointer;
`;

const Back = ({ history }) => {
  const goBack = () => history.goBack();
  return (
    <BackWrap onClick={goBack}>
      <FontAwesomeIcon icon={faChevronLeft} />
    </BackWrap>
  );
};

export const BurgerMenu = ({
  history,
  match: {
    params: { route }
  }
}) => {
  return (
    <>
      <Menu>
        <Link className="menu-item" to="/">
          Home
        </Link>
        <Link className="menu-item" to="/create">
          Create
        </Link>
        <Link className="menu-item" to="/meetings">
          Meetings
        </Link>
        <Link className="menu-item" to="/notes">
          Notes
        </Link>
        <Link className="menu-item" to="/contacts">
          Contacts
        </Link>
        <Link className="menu-item" to="/stats">
          Stats
        </Link>
      </Menu>
      {route && <Back history={history} />}
    </>
  );
};

export default decorator(BurgerMenu);
